defmodule Ejabberd.ConfigFile do
  use Ejabberd.Config

  def start do
    [loglevel: 4,
     language: "en",
     hosts: ["localhost"]]
  end

  module :mod_time do
    @attr_not_supported true
  end

  module :mod_configure do
    @dependency [:mod_adhoc]
  end

  module :mod_irc do
  end
end
