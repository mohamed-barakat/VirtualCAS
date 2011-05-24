#############################################################################
##
##  read.g                                                VirtualCAS package
##
##  Copyright 2011, Mohamed Barakat, University of Kaiserslautern
##                 Andreas Steenpaß, University of Kaiserslautern
##
##  Reading the implementation part of the VirtualCAS package.
##
#############################################################################

## init
ReadPackage( "VirtualCAS", "gap/VirtualCASForHomalg.gi" );

## init
ReadPackage( "VirtualCAS", "gap/VirtualCAS.gi" );

## the basic matrix operations
ReadPackage( "VirtualCAS", "gap/VirtualCASBasic.gi" );

## the matrix tool operations
ReadPackage( "VirtualCAS", "gap/VirtualCASTools.gi" );

## build the homalgTable
ReadPackage( "VirtualCAS", "gap/VirtualCASTable.gi" );

