import Config

config :schoolhub,
  db_backend: Schoolhub.DataManagerMock,
  xmpp_backend: Schoolhub.RomeoMock,

  reg_admin_jid: "admin@localhost",
  reg_admin_pw: "admin"
