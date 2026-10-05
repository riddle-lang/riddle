use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TargetTriple {
    X86_64UnknownLinuxGnu,
    Aarch64UnknownLinuxGnu,
    I686UnknownLinuxGnu,
    X86_64PcWindowsMsvc,
    I686PcWindowsMsvc,
    Aarch64PcWindowsMsvc,
    Aarch64AppleDarwin,
}

impl TargetTriple {
    pub const ALL: [Self; 7] = [
        Self::X86_64UnknownLinuxGnu,
        Self::Aarch64UnknownLinuxGnu,
        Self::I686UnknownLinuxGnu,
        Self::X86_64PcWindowsMsvc,
        Self::I686PcWindowsMsvc,
        Self::Aarch64PcWindowsMsvc,
        Self::Aarch64AppleDarwin,
    ];

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::X86_64UnknownLinuxGnu => "x86_64-unknown-linux-gnu",
            Self::Aarch64UnknownLinuxGnu => "aarch64-unknown-linux-gnu",
            Self::I686UnknownLinuxGnu => "i686-unknown-linux-gnu",
            Self::X86_64PcWindowsMsvc => "x86_64-pc-windows-msvc",
            Self::I686PcWindowsMsvc => "i686-pc-windows-msvc",
            Self::Aarch64PcWindowsMsvc => "aarch64-pc-windows-msvc",
            Self::Aarch64AppleDarwin => "aarch64-apple-darwin",
        }
    }

    #[must_use]
    pub const fn operating_system(self) -> &'static str {
        match self {
            Self::X86_64UnknownLinuxGnu
            | Self::Aarch64UnknownLinuxGnu
            | Self::I686UnknownLinuxGnu => "linux",
            Self::X86_64PcWindowsMsvc | Self::I686PcWindowsMsvc | Self::Aarch64PcWindowsMsvc => {
                "windows"
            }
            Self::Aarch64AppleDarwin => "macos",
        }
    }

    #[must_use]
    pub const fn executable_suffix(self) -> &'static str {
        if self.is_windows() { ".exe" } else { "" }
    }

    #[must_use]
    pub const fn msvc_architecture(self) -> &'static str {
        match self {
            Self::X86_64PcWindowsMsvc => "x64",
            Self::I686PcWindowsMsvc => "x86",
            Self::Aarch64PcWindowsMsvc => "arm64",
            _ => "",
        }
    }

    /// Width, in bits, of `size_t` on this target — the width `usize` and
    /// `isize` are measured in, so an integer literal is range-checked against
    /// the machine the program is built for rather than the one building it.
    #[must_use]
    pub const fn pointer_width_bits(self) -> u32 {
        match self {
            Self::I686UnknownLinuxGnu | Self::I686PcWindowsMsvc => 32,
            Self::X86_64UnknownLinuxGnu
            | Self::Aarch64UnknownLinuxGnu
            | Self::X86_64PcWindowsMsvc
            | Self::Aarch64PcWindowsMsvc
            | Self::Aarch64AppleDarwin => 64,
        }
    }

    #[must_use]
    pub const fn is_linux(self) -> bool {
        matches!(
            self,
            Self::X86_64UnknownLinuxGnu | Self::Aarch64UnknownLinuxGnu | Self::I686UnknownLinuxGnu
        )
    }

    #[must_use]
    pub const fn is_windows(self) -> bool {
        matches!(
            self,
            Self::X86_64PcWindowsMsvc | Self::I686PcWindowsMsvc | Self::Aarch64PcWindowsMsvc
        )
    }

    #[must_use]
    pub const fn is_macos(self) -> bool {
        matches!(self, Self::Aarch64AppleDarwin)
    }

    /// Detects the target represented by the current compiler host.
    ///
    /// # Errors
    ///
    /// Returns [`UnsupportedTarget`] when the host triple is not supported.
    pub fn host() -> Result<Self, UnsupportedTarget> {
        Self::from_host(
            std::env::consts::OS,
            std::env::consts::ARCH,
            if cfg!(target_env = "msvc") {
                "msvc"
            } else if cfg!(target_env = "gnu") {
                "gnu"
            } else {
                ""
            },
        )
    }

    /// Maps host platform components to a supported target triple.
    ///
    /// # Errors
    ///
    /// Returns [`UnsupportedTarget`] when the combination is not supported.
    pub fn from_host(os: &str, arch: &str, environment: &str) -> Result<Self, UnsupportedTarget> {
        let triple = match (os, arch, environment) {
            ("linux", "x86_64", "gnu") => Self::X86_64UnknownLinuxGnu,
            ("linux", "aarch64", "gnu") => Self::Aarch64UnknownLinuxGnu,
            ("linux", "x86" | "i686", "gnu") => Self::I686UnknownLinuxGnu,
            ("windows", "x86_64", "msvc") => Self::X86_64PcWindowsMsvc,
            ("windows", "x86" | "i686", "msvc") => Self::I686PcWindowsMsvc,
            ("windows", "aarch64", "msvc") => Self::Aarch64PcWindowsMsvc,
            ("macos", "aarch64", "") => Self::Aarch64AppleDarwin,
            _ => {
                return Err(UnsupportedTarget(format!(
                    "unsupported host platform `{os}/{arch}/{environment}`"
                )));
            }
        };
        Ok(triple)
    }
}

impl fmt::Display for TargetTriple {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl FromStr for TargetTriple {
    type Err = UnsupportedTarget;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::ALL
            .into_iter()
            .find(|target| target.as_str() == value)
            .ok_or_else(|| {
                UnsupportedTarget(format!(
                    "unsupported target `{value}`; supported targets: {}",
                    Self::ALL.map(Self::as_str).join(", ")
                ))
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnsupportedTarget(String);

impl fmt::Display for UnsupportedTarget {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for UnsupportedTarget {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_exactly_the_supported_targets() {
        for target in TargetTriple::ALL {
            assert_eq!(target.as_str().parse::<TargetTriple>().unwrap(), target);
        }
        assert!("x86_64-unknown-linux-musl".parse::<TargetTriple>().is_err());
        assert!("x86_64-pc-windows-gnu".parse::<TargetTriple>().is_err());
        assert!("x86_64-apple-darwin".parse::<TargetTriple>().is_err());
    }

    #[test]
    fn exposes_target_output_properties() {
        assert_eq!(
            TargetTriple::X86_64PcWindowsMsvc.executable_suffix(),
            ".exe"
        );
        assert_eq!(TargetTriple::Aarch64UnknownLinuxGnu.executable_suffix(), "");
    }

    #[test]
    fn maps_supported_host_parts_to_full_triples() {
        assert_eq!(
            TargetTriple::from_host("windows", "x86_64", "msvc").unwrap(),
            TargetTriple::X86_64PcWindowsMsvc
        );
        assert_eq!(
            TargetTriple::from_host("linux", "x86", "gnu").unwrap(),
            TargetTriple::I686UnknownLinuxGnu
        );
        assert!(TargetTriple::from_host("macos", "x86_64", "").is_err());
    }
}
