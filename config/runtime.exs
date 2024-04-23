import Config

rootdefault = case System.get_env("RELIVE", "false") do
  "true" -> "_build/relive"
  "false" -> ""
end

rootpath = System.get_env("RELEASE_ROOT", rootdefault)
config :ejabberd,
  file: Path.join(rootpath, "conf/ejabberd.yml"),
  log_path: Path.join(rootpath, "logs/ejabberd.log")
config :mnesia,
  dir: Path.join(rootpath, "database/")
config :exsync,
  reload_callback: {:ejabberd_admin, :update, []}
