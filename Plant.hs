

module Main where

import SP
import SPHelper
import MonadLib
import MakeCore
import Make
import Dep
import MakeHelper
import MakeSpec
import Misc
import System
import List
import Maybe
import IO
import Path

import Monad

pkgs = map ("-package "++) ["data","util","gtkhs","HOpenGL"]

fs = map Opt ("-fglasgow-exts":"-fallow-overlapping-instances":pkgs)
  ++ map Opt ["'-#include <gtkgl/gtkglarea.h>'","'-#include <libglade-1.0/glade/glade.h>'"]
  ++ map Link ["png","gtkgl","gthread","gtk","gdk","glib"]

rs = " -geometry 1600x1200+200+0 "

gui = runG_ (exec " gui/gui.glade GuiParts" <-< hs_make "gui/glade2hs" [Opt "-package lang -package text"] "gui/glade2hs.hs" <-< nullSP)

targets = 
    ("plantlab", runG_ (plantlab <-< nullSP))
  : ("gui", gui)
  : ("run", runG_ (exec rs <-< plantlab <-< nullSP))
  : ("run_", runG_ (exec rs <-< putSP (MkExec future "plantlab") idSP <-< nullSP))
  : ("plg", runG_ (hs_make_ fs "Plants2.hs" <-< nullSP))
  : ("simul", runG_ (hs_make "simul" fs "TestSim.hs" <-< nullSP))
  : []  

plantlab = hs_make "plantlab" fs "Main.hs" <-< object "png/png.o"

main = run targets


