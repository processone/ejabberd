-type matchspec_atom() :: '_' | '$1' | '$2' | '$3'.
-record(carboncopy, {us       :: {binary(), binary()} | matchspec_atom(), 
		     resource :: binary() | matchspec_atom(),
		     version  :: binary() | matchspec_atom()}).
