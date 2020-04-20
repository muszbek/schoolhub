{node, tester, 'tester@10.3.0.9'}.
{node, client1, 'client1@10.3.0.10'}.
{node, client2, 'client2@10.3.0.10'}.
{node, client3, 'client3@10.3.0.10'}.
{node, client4, 'client4@10.3.0.10'}.
{node, client5, 'client5@10.3.0.10'}.
{node, client6, 'client6@10.3.0.10'}.
{node, client7, 'client7@10.3.0.10'}.
{node, client8, 'client8@10.3.0.10'}.
{node, client9, 'client9@10.3.0.10'}.
{node, client10, 'client10@10.3.0.10'}.

{logdir, master, "./logs_master/"}.
{logdir, "/root/test/logs_dist/"}.

{suites, tester, "/root/test/suites_local/", all}.
{suites, tester, "/root/test/suites_dist/", all}.
{suites, "/root/test/util/", all}.

{config, "/root/test/dist.config"}.
