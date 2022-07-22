%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2021 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2021 ProcessOne, SARL.
%%% All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(eturnal_cert).
-export([create/1]).

-include_lib("public_key/include/public_key.hrl").
-define(ETURNAL_KEY_SIZE, 4096).

% Currently not exported by calendar:
-type year() :: non_neg_integer().
-type month() :: 1..12.
-type day() :: 1..31.

%% API.

-spec create(string() | binary()) -> binary().
create(Domain) when is_binary(Domain) ->
    create(binary_to_list(Domain));
create(Domain) ->
    Key = private_key(),
    Crt = certificate(Domain, Key),
    public_key:pem_encode([pem_entry(Key), pem_entry(Crt)]).

%% Internal functions.

-spec private_key() -> #'RSAPrivateKey'{}.
private_key() ->
    public_key:generate_key({rsa, ?ETURNAL_KEY_SIZE, 65537}).

-spec certificate(string(), #'RSAPrivateKey'{}) -> public_key:der_encoded().
certificate(Domain, Key) ->
    TBS = #'OTPTBSCertificate'{
             serialNumber = serial_number(),
             signature = signature(),
             issuer = issuer(Domain),
             validity = validity(),
             subject = subject(Domain),
             subjectPublicKeyInfo = subject_key_info(Key),
             extensions = extensions(Domain)},
    public_key:pkix_sign(TBS, Key).

-spec serial_number() -> pos_integer().
serial_number() ->
    rand:uniform(1000000000).

-spec signature() -> #'SignatureAlgorithm'{}.
signature() ->
    #'SignatureAlgorithm'{
       algorithm = ?'sha256WithRSAEncryption',
       parameters = 'NULL'}.

-spec issuer(string()) -> {rdnSequence, [[#'AttributeTypeAndValue'{}]]}.
issuer(Domain) -> % Self-signed.
    subject(Domain).

-spec validity() -> #'Validity'{}.
validity() ->
    #'Validity'{
       notBefore = format_date(calendar:universal_time()),
       notAfter = format_date(2038, 1, 1)}.

-spec subject(string()) -> {rdnSequence, [[#'AttributeTypeAndValue'{}]]}.
subject(Domain) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{
          type = ?'id-at-commonName',
          value = {printableString, Domain}}]]}.

-spec subject_key_info(#'RSAPrivateKey'{}) -> #'OTPSubjectPublicKeyInfo'{}.
subject_key_info(#'RSAPrivateKey'{modulus = Modulus, publicExponent = Exp}) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm =
           #'PublicKeyAlgorithm'{
              algorithm = ?'rsaEncryption',
              parameters = 'NULL'},
       subjectPublicKey =
           #'RSAPublicKey'{
              modulus = Modulus,
              publicExponent = Exp}}.

-spec extensions(string()) -> [#'Extension'{}].
extensions(Domain) ->
    [#'Extension'{
        extnID = ?'id-ce-subjectAltName',
        extnValue = [{dNSName, Domain}],
        critical = false},
     #'Extension'{
        extnID = ?'id-ce-basicConstraints',
        extnValue = #'BasicConstraints'{cA = true},
        critical = false}].

-spec pem_entry(#'RSAPrivateKey'{} | public_key:der_encoded())
      -> public_key:pem_entry().
pem_entry(#'RSAPrivateKey'{} = Key) ->
    public_key:pem_entry_encode('RSAPrivateKey', Key);
pem_entry(Crt) when is_binary(Crt) ->
    {'Certificate', Crt, not_encrypted}.

-spec format_date(year(), month(), day()) -> {utcTime, string()}.
format_date(Y, M, D) when Y >= 2000 ->
    {utcTime,
     lists:flatten(
       io_lib:format("~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0BZ",
                     [Y - 2000, M, D, 0, 0, 0]))}.

-spec format_date(calendar:datetime()) -> {utcTime, string()}.
format_date({{Y, M, D}, {_, _, _}}) ->
    format_date(Y, M, D).
