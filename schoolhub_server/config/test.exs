import Config

config :schoolhub,
  db_backend: Schoolhub.DataManagerMock,
  db_content_backend: Schoolhub.ContentManagerMock,
  xmpp_backend: Schoolhub.RomeoMock,
  admin_password: "admin"
