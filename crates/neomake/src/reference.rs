use {
    crate::args::ClapArgumentLoader,
    anyhow::Result,
    clap_complete::Shell,
    clap_mangen::Man,
    std::{
        fs::File,
        io::Write,
        path::Path,
    },
};

fn collect_commands() -> Vec<(String, clap::Command)> {
    fn recurse(path: &str, out: &mut Vec<(String, clap::Command)>, parent: &clap::Command) {
        let new_path = format!("{}-{}", path, parent.get_name());
        out.push((new_path.clone(), parent.clone()));
        for sub in parent.get_subcommands() {
            recurse(&new_path, out, sub);
        }
    }
    let mut out = Vec::<(String, clap::Command)>::new();
    recurse("", &mut out, &ClapArgumentLoader::root_command());
    out
}

pub fn build_shell_completion(outdir: &Path, shell: &Shell) -> Result<()> {
    let mut app = ClapArgumentLoader::root_command();
    clap_complete::generate_to(*shell, &mut app, "neomake", outdir)?;
    Ok(())
}

pub fn build_markdown(outdir: &Path) -> Result<()> {
    for (name, cmd) in collect_commands() {
        let path = outdir.join(format!("{}.md", name.trim_start_matches('-')));
        let mut f = File::create(&path)?;
        f.write_all(clap_markdown::help_markdown_command(&cmd).as_bytes())?;
    }
    Ok(())
}

pub fn build_manpages(outdir: &Path) -> Result<()> {
    for (name, cmd) in collect_commands() {
        let path = outdir.join(format!("{}.1", name.trim_start_matches('-')));
        let mut f = File::create(&path)?;
        Man::new(cmd).render(&mut f)?;
    }
    Ok(())
}
