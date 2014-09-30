module GUID where

foreign data "java.util.UUID" GUID
        function "java.util.UUID" "randomUUID" guid : IO GUID
        method "toString" guidString : GUID -> String

