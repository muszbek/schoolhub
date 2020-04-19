{node, tester, 'tester@10.3.0.9'}.
{node, client1, 'client1@10.3.0.10'}.
{node, client2, 'client2@10.3.0.10'}.

{logdir, master, "./logs_master/"}.
{logdir, "/root/test/logs_dist/"}.

{suites, tester, "/root/test/suites_local/", all}.
{suites, tester, "/root/test/suites_dist/", all}.
{suites, "/root/test/util/", all}.

{config, "/root/test/dist.config"}.
