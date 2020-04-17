{logdir, "./logs/"}.
{logdir, master, "./master_logs/"}.

{suites, tester, ".", all}.

{node, tester, 'tester@10.0.2.15'}.
{node, client1, 'client1@10.0.2.15'}.
{node, client2, 'client2@10.0.2.15'}.

{init, tester, [{node_start, [{monitor_master, true}]}]}.
{init, [client1, client2],
 [{node_start, [{monitor_master, true},
		{startup_functions,
		 [{application, ensure_all_started, [schoolhub_client]}]}]}]}.
