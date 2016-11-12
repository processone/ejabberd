defmodule Ejabberd.ConfigFile do
  use Ejabberd.Config

  def start do
    [loglevel: 4,
     language: "en",
     hosts: ["localhost"]]
  end
end
