use Mix.Config

# This is standard path in the context of ejabberd release
config :ejabberd,
  file: "config/ejabberd.yml",
  log_path: 'log/ejabberd.log'

# Customize Mnesia directory:
config :mnesia,
  dir: 'database/'
