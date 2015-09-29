defmodule Ejabberd.Logger do

  def critical(message, args \\ []), do: :lager.log(:critical, [], message, args)
  def error(message, args \\ []),    do: :lager.log(:error, [], message, args)
  def warning(message, args \\ []),  do: :lager.log(:warning, [], message, args)
  def info(message, args \\ []),     do: :lager.log(:info, [], message, args)
  def debug(message, args \\ []),    do: :lager.log(:debug, [], message, args)

end
