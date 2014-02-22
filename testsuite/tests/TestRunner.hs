
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

import ErpModel
import ErpServer(serverMain)

main = serverMain "./dist/build/tests"
