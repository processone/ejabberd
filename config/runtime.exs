import Config

case System.get_env("RELIVE", "false") do
  "true" ->
    rootpath = System.get_env("RELEASE_ROOT", "_build/relive")
    config :ejabberd,
      file: Path.join(rootpath, "conf/ejabberd.yml"),
      log_path: Path.join(rootpath, 'logs/ejabberd.log')
    config :mnesia,
      dir: Path.join(rootpath, 'database/')
  "false" ->
    rootpath = System.get_env("RELEASE_ROOT", "")
    config :ejabberd,
      file: Path.join(rootpath, "etc/ejabberd/ejabberd.yml"),
      log_path: Path.join(rootpath, 'var/log/ejabberd/ejabberd.log')
    config :mnesia,
      dir: Path.join(rootpath, 'var/lib/ejabberd/')
end
