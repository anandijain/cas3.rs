pub struct ValkyrieConfig {
    pub running_time: bool,
    pub image_max_pixel: usize,
}

impl Default for ValkyrieConfig {
    fn default() -> Self {
        ValkyrieConfig { running_time: false, image_max_pixel: 1024 * 768 }
    }
}
