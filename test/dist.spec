{node, tester, 'tester@10.0.2.15'}.
{node, client1, 'client1@10.3.0.10'}.
{node, client2, 'client2@10.3.0.10'}.

{logdir, master, "./logs_master/"}.
{logdir, "/root/test/logs_dist/"}.
{logdir, tester, "./logs_dist/"}.

%{suites, tester, "./local/", all}.
{suites, tester, "./dist/", all}.
{suites, tester, "./util/", all}.

%{init, tester, [{node_start, [{monitor_master, true}]}]}.
%{init, [client1, client2],
% [{node_start, [{monitor_master, true},
%		{startup_functions,
%		 [{application, ensure_all_started, [schoolhub_client]}]}]}]}.
