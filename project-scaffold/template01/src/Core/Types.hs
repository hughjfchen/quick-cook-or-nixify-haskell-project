-- | This module defines the core domain types which shared by lib and exe

module Core.Types(
  SwitchName(..)
  , Switch(..)
  , JvmOptionName(..)
  , Option(..)
  , Property(..)
  , JVMCmdLine(..)
  , toJVMCmdLine
            ) where


data SwitchName = VerboseGC
                | VerboseClass
                | VerboseModule
                | VerboseJNI
                | Xint
                | Xrs
                | Xnoclassgc
                deriving stock (Show)

newtype Switch = Switch SwitchName deriving stock (Show)

showSwitch :: Switch -> Text
showSwitch (Switch VerboseGC) = "verbose:gc"
showSwitch (Switch VerboseClass) = "verbose:class"
showSwitch (Switch VerboseModule) = "verbose:module"
showSwitch (Switch VerboseJNI) = "verbose:jni"
showSwitch (Switch Xint) = "Xint"
showSwitch (Switch Xrs) = "Xrs"
showSwitch (Switch Xnoclassgc) = "Xnoclassgc"

data JvmOptionName = Xms
                | Xmx
                | Xloggc
                deriving stock (Show)
type JvmOptionValue = Text
data JvmOption = JvmOption JvmOptionName JvmOptionValue deriving stock (Show)

showJvmOption :: JvmOption -> Text
showJvmOption (JvmOption Xms v) = "Xms" <> v
showJvmOption (JvmOption Xmx v) = "Xmx" <> v
showJvmOption (JvmOption Xloggc v) = "Xloggc:" <> v

type PropertyName = Text
type PropertyValue = Text
data Property = Property PropertyName PropertyValue

data JVMCmdLine = JVMCmdLineSwitch Switch
                | JVMCmdLineJvmOption JvmOption
                | JVMCmdLineProperty Property

toJVMCmdLine :: JVMCmdLine -> Text
toJVMCmdLine (JVMCmdLineSwitch s) = "-" <> showSwitch s
toJVMCmdLine (JVMCmdLineJvmOption (JvmOption _ "")) = ""
toJVMCmdLine (JVMCmdLineJvmOption o) = "-" <> showJvmOption o

toJVMCmdLine (JVMCmdLineProperty (Property "" "")) = ""
toJVMCmdLine (JVMCmdLineProperty (Property "" _)) = ""
toJVMCmdLine (JVMCmdLineProperty (Property _ "")) = ""
toJVMCmdLine (JVMCmdLineProperty (Property n v)) = "-D"
                                        <> n
                                        <> "="
                                        <> v
