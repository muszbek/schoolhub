import Config

config :schoolhub,
  db_backend: Schoolhub.DataManagerMock,
  xmpp_backend: Schoolhub.RomeoMock
