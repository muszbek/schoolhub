{node, tester, 'tester@10.0.2.15'}.
{node, client1, 'client1@10.3.0.10'}.
{node, client2, 'client2@10.3.0.10'}.

{logdir, master, "./master_logs/"}.
{logdir, "/root/test/logs/"}.
{logdir, tester, "./logs/"}.

{suites, tester, ".", all}.

%{init, tester, [{node_start, [{monitor_master, true}]}]}.
%{init, [client1, client2],
% [{node_start, [{monitor_master, true},
%		{startup_functions,
%		 [{application, ensure_all_started, [schoolhub_client]}]}]}]}.
