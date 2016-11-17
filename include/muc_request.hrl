%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_request.xdata
%% Form type: http://jabber.org/protocol/muc#request
%% Document: XEP-0045


-type property() :: {'role', participant} |
                    {'jid', jid:jid()} |
                    {'roomnick', binary()} |
                    {'request_allow', boolean()}.
-type result() :: [property()].

-type options(T) :: [{binary(), T}].
-type property_with_options() ::
      {'role', participant, options(participant)}.
-type form() :: [property() | property_with_options() | xdata_field()].
