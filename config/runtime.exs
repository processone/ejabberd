import Config

rootpath = System.get_env("RELEASE_ROOT", "")

# This is standard path in the context of ejabberd release
config :ejabberd,
  file: Path.join(rootpath, "etc/ejabberd/ejabberd.yml"),
  log_path: Path.join(rootpath, 'var/log/ejabberd/ejabberd.log')

# Customize Mnesia directory:
config :mnesia,
  dir: Path.join(rootpath, 'var/lib/ejabberd/')
