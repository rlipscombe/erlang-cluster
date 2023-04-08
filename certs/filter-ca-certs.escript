#!/usr/bin/env escript

-include_lib("public_key/include/public_key.hrl").

main([Input]) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    {ok, Pem} = file:read_file(Input),
    PemEntries = public_key:pem_decode(Pem),
    ValidEntries = lists:filter(fun({_, Der, _}) ->
        Cert = public_key:pkix_decode_cert(Der, otp),
        Validity = Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.validity,
        NotAfter = pubkey_cert:time_str_2_gregorian_sec(Validity#'Validity'.notAfter),
        NotAfter >= Now
    end, lists:uniq(PemEntries)),
    io:put_chars(public_key:pem_encode(ValidEntries)),
    ok.
