use std::fs::File;
use std::io;
use std::os::windows::io::AsRawHandle;

use windows_sys::Win32::Foundation::HANDLE;
use windows_sys::Win32::Storage::FileSystem::{
    FileBasicInfo, FileIdInfo, FileStandardInfo, GetFileInformationByHandleEx, FILE_BASIC_INFO,
    FILE_ID_INFO, FILE_INFO_BY_HANDLE_CLASS, FILE_STANDARD_INFO,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct WindowsFileInformation {
    pub(crate) volume: u64,
    pub(crate) file: [u8; 16],
    pub(crate) links: u32,
    pub(crate) attributes: u32,
    pub(crate) size: u64,
    pub(crate) creation_time: i64,
    pub(crate) last_write_time: i64,
    pub(crate) change_time: i64,
    pub(crate) directory: bool,
    pub(crate) delete_pending: bool,
}

pub(crate) fn file_information(file: &File) -> io::Result<WindowsFileInformation> {
    let handle = file.as_raw_handle() as HANDLE;
    let identity = query::<FILE_ID_INFO>(handle, FileIdInfo)?;
    let basic = query::<FILE_BASIC_INFO>(handle, FileBasicInfo)?;
    let standard = query::<FILE_STANDARD_INFO>(handle, FileStandardInfo)?;
    let size = u64::try_from(standard.EndOfFile).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Windows reported a negative file size",
        )
    })?;
    Ok(WindowsFileInformation {
        volume: identity.VolumeSerialNumber,
        file: identity.FileId.Identifier,
        links: standard.NumberOfLinks,
        attributes: basic.FileAttributes,
        size,
        creation_time: basic.CreationTime,
        last_write_time: basic.LastWriteTime,
        change_time: basic.ChangeTime,
        directory: standard.Directory,
        delete_pending: standard.DeletePending,
    })
}

fn query<T: Default>(handle: HANDLE, class: FILE_INFO_BY_HANDLE_CLASS) -> io::Result<T> {
    let mut information = T::default();
    let size = u32::try_from(std::mem::size_of::<T>())
        .expect("Windows file information structures fit in u32");
    let succeeded = unsafe {
        GetFileInformationByHandleEx(
            handle,
            class,
            std::ptr::from_mut(&mut information).cast(),
            size,
        )
    };
    if succeeded == 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(information)
}
