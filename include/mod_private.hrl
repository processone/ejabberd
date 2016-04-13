-record(private_storage,
        {usns = {<<"">>, <<"">>, <<"">>} :: {binary(), binary(), binary() |
                                             '$1' | '_'},
         xml = #xmlel{} :: xmlel() | '_' | '$1'}).
