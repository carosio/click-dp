CLICK DP API Sample
===================

This is the scg_session CLICK DP API wrapper as standalone application for test and development purposes.

Start with node local mock DP:

    $ erl -sname click-dp -setcookie secret
    Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

    Eshell V6.2  (abort with ^G)
    (click-dp@alice)1> application:ensure_all_started(scg_click_dp).

Start with node real DP:

    $ erl -sname click -setcookie secret
    Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

    Eshell V6.2  (abort with ^G)
    (click@alice)1> application:ensure_all_started(scg_click_dp).
