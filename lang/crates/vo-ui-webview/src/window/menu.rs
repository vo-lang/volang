use super::Wake;
use muda::{
    accelerator::{Accelerator, Code, Modifiers},
    MenuEvent, MenuItem, PredefinedMenuItem as Item, Submenu,
};
use tao::event_loop::EventLoopProxy;

/// macOS routes editing shortcuts through the application menu. The custom
/// quit action goes through normal guest cleanup instead of terminating NSApp.
pub(super) struct Menu(muda::Menu);

impl Menu {
    pub fn install(title: &str, proxy: EventLoopProxy<Wake>) -> Result<Self, String> {
        let quit = MenuItem::new(
            format!("Quit {title}"),
            true,
            Some(Accelerator::new(Some(Modifiers::SUPER), Code::KeyQ)),
        );
        let app = Submenu::with_items(
            title,
            true,
            &[
                &Item::hide(None),
                &Item::hide_others(None),
                &Item::show_all(None),
                &Item::separator(),
                &quit,
            ],
        )
        .map_err(|error| error.to_string())?;
        let edit = Submenu::with_items(
            "Edit",
            true,
            &[
                &Item::undo(None),
                &Item::redo(None),
                &Item::separator(),
                &Item::cut(None),
                &Item::copy(None),
                &Item::paste(None),
                &Item::select_all(None),
            ],
        )
        .map_err(|error| error.to_string())?;
        let window = Submenu::with_items(
            "Window",
            true,
            &[&Item::minimize(None), &Item::close_window(None)],
        )
        .map_err(|error| error.to_string())?;
        let menu =
            muda::Menu::with_items(&[&app, &edit, &window]).map_err(|error| error.to_string())?;
        let quit = quit.id().clone();
        MenuEvent::set_event_handler(Some(move |event: MenuEvent| {
            if event.id == quit {
                let _ = proxy.send_event(Wake::Close);
            }
        }));
        menu.init_for_nsapp();
        Ok(Self(menu))
    }
}

impl Drop for Menu {
    fn drop(&mut self) {
        self.0.remove_for_nsapp();
        MenuEvent::set_event_handler(None::<fn(MenuEvent)>);
    }
}
