defmodule Ejabberd.Logger do

  def critical(message, args \\ []), do: :logger.critical(message, args)
  def error(message, args \\ []),    do: :logger.error(message, args)
  def warning(message, args \\ []),  do: :logger.warning(message, args)
  def info(message, args \\ []),     do: :logger.info(message, args)
  def debug(message, args \\ []),    do: :logger.debug( message, args)

end
