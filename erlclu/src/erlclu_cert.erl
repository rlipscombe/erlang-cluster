-module(erlclu_cert).
-export([
    read_certificate/1,
    convert_certificate/1
]).

-include_lib("public_key/include/public_key.hrl").

read_certificate(CertFile) ->
    {ok, Encoded} = file:read_file(CertFile),
    Decoded = public_key:pem_decode(Encoded),
    Entries = [public_key:pem_entry_decode(Entry) || Entry <- Decoded],
    Entries.

convert_certificate(Cert = #'Certificate'{}) ->
    convert_certificate(Cert#'Certificate'.tbsCertificate);
convert_certificate(Cert = #'TBSCertificate'{}) ->
    Issuer = convert_rdn_sequence(Cert#'TBSCertificate'.issuer),
    Subject = convert_rdn_sequence(Cert#'TBSCertificate'.subject),
    NotBefore = convert_validity_entry(Cert#'TBSCertificate'.validity#'Validity'.notBefore),
    NotAfter = convert_validity_entry(Cert#'TBSCertificate'.validity#'Validity'.notAfter),
    #{issuer => Issuer, subject => Subject, not_before => NotBefore, not_after => NotAfter}.

convert_rdn_sequence({rdnSequence, Attributes}) ->
    lists:flatten(lists:join("/", lists:map(fun convert_rdn_attribute/1, Attributes))).

convert_rdn_attribute([#'AttributeTypeAndValue'{type = Type, value = Value}]) ->
    [convert_attribute_type(Type), "=", convert_attribute_value(Value)].

convert_attribute_type(?'id-at-countryName') -> "C";
convert_attribute_type(?'id-at-localityName') -> "L";
convert_attribute_type(?'id-at-organizationName') -> "O";
convert_attribute_type(?'id-at-commonName') -> "CN".

convert_attribute_value(<<12, Length:8, Value:Length/binary>>) ->
    binary_to_list(Value);
convert_attribute_value(<<19, Length:8, Value:Length/binary>>) ->
    binary_to_list(Value).

convert_validity_entry(TimeStr) ->
    Secs = pubkey_cert:time_str_2_gregorian_sec(TimeStr),
    calendar:gregorian_seconds_to_datetime(Secs).
