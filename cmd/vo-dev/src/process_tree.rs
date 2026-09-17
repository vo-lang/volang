//! Owned native process group/job. Cancellation and drop reap the full tree.
use process_wrap::std::*;
use std::process::{Command, ExitStatus};

pub(crate) struct ProcessTree {
    child: Box<dyn ChildWrapper>,
    reaped: bool,
}

impl ProcessTree {
    pub(crate) fn spawn(command: Command) -> std::io::Result<Self> {
        let mut command = CommandWrap::from(command);
        #[cfg(unix)]
        command.wrap(ProcessGroup::leader());
        #[cfg(windows)]
        command.wrap(JobObject);
        Ok(Self {
            child: command.spawn()?,
            reaped: false,
        })
    }

    /// A completed root must not leave its descendants running.
    pub(crate) fn try_wait(&mut self) -> std::io::Result<Option<ExitStatus>> {
        let status = self.child.try_wait()?;
        if status.is_some() {
            let _ = self.child.start_kill();
            self.reaped = true;
        }
        Ok(status)
    }

    pub(crate) fn terminate(&mut self) -> std::io::Result<ExitStatus> {
        self.child.start_kill()?;
        let status = self.wait()?;
        self.reaped = true;
        Ok(status)
    }

    fn wait(&mut self) -> std::io::Result<ExitStatus> {
        // JobObject::try_wait may have drained the completion port already.
        // Termination still owns the whole job; reap the root without waiting
        // for another notification that Windows need not send.
        #[cfg(windows)]
        return self.child.inner_mut().wait();
        #[cfg(not(windows))]
        self.child.wait()
    }
}

impl Drop for ProcessTree {
    fn drop(&mut self) {
        if !self.reaped {
            let _ = self.child.start_kill();
            let _ = self.wait();
        }
    }
}
