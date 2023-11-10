pub struct Cas3Config {
    pub running_time: bool,
    pub image_max_pixel: usize,
}

impl Default for Cas3Config {
    fn default() -> Self {
        Cas3Config { running_time: false, image_max_pixel: 1024 * 768 }
    }
}
