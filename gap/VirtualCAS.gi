#############################################################################
##
##  VirtualCAS.gd                     VirtualCAS package
##
##  Copyright 2011, Mohamed Barakat, University of Kaiserslautern
##
##  Implementation stuff for VirtualCAS.
##
#############################################################################

####################################
#
# global variables:
#
####################################

InstallValue( VirtualCAS,
        rec(
            task_name := "homalg_task_",
            proc_name := "homalg_proc",
            )
);

####################################
#
# initialization
#
####################################

HOMALG_IO_Singular.LaunchCAS := LaunchVirtualCAS;

####################################
#
# global functions and variables:
#
####################################

##
InstallValue( VirtualCASMacrosForSingular,
        rec(
            
            _CAS_name := "Singular",
            
            _Identifier := "VirtualCAS",
            
            ("!init_string_VirtualCAS") := "LIB \"tasks.lib\"",
            
            )
        
        );

##
UpdateMacrosOfCAS( VirtualCASMacrosForSingular, SingularMacros );
UpdateMacrosOfLaunchedCASs( VirtualCASMacrosForSingular );

####################################
#
# methods for operations:
#
####################################

##
InstallGlobalFunction( LaunchVirtualCAS,
  function( arg )
    local s;
    
    if LoadPackage( "IO_ForHomalg" ) <> true then
        Error( "the package IO_ForHomalg failed to load\n" );
    fi;
    
    s := LaunchCAS_IO_ForHomalg( HOMALG_IO_Singular );
    
    if s = fail then
        return fail;
    fi;
    
    s.lines := "";
    s.errors := "";
    s.name := "virtual CAS running Singular";
    s.InitialSendBlockingToCAS := s.SendBlockingToCAS;
    s.SendBlockingToCAS := SendBlockingToVirtualCAS;
    s.SendBlockingToCAS_original := SendBlockingToVirtualCAS;
    s.log_processes := true;
    
    s.ProcessGraph := ProcessGraph( );
    
    return s;
    
end );

CreateHPCString := function( stream, process )
  local id, parents, str, executed, graph, p, v, t, hv, wA;
  
  id := process!.identifier;
  
  parents := process!.parents;
  
  str := process!.string;
  
  executed := [ ];
  
  if not IsBlockingProcess( process ) and
     Length( id ) = 1 and parents <> [ ] then
      
      #Print( id, "\n" );
      
      id := String( id[1] );
      
      p := VirtualCAS.proc_name;
      v := stream!.variable_name;
      t := VirtualCAS.task_name;
      
      str := Concatenation(
                     "proc ", p, "{\n",
                     str,
                     "return(", v, id, ");}\n",
                     "task ", t, id, "=", "\"", p, "\", list();\n",
                     "startTasks(", t, id, ");\n"
                     );
      
      graph := stream.ProcessGraph;
      
      parents := Difference( parents, graph!.parentless );
      executed := Difference( parents, graph!.executed );
      
      if executed <> [ ] then
          
          wA := List( executed, i -> Concatenation( t, String( i ) ) );
          wA := JoinStringsWithSeparator( wA );
          
          hv := List( executed, i -> Concatenation( "def ", v, String( i ), "=getResult(", t, String( i ), "); killTask(", t, String( i ), ");" ) );
          hv := Concatenation( hv );
          
          str := Concatenation(
                         "waitAllTasks(", wA, ");\n",
                         hv, "\n",
                         str
                         );
          
      fi;
      
  elif not IsBlockingProcess( process ) and
    Length( id ) > 1 and parents <> [ ] then
      
      graph := stream.ProcessGraph;
      
      parents := Difference( parents, graph!.parentless );
      executed := Difference( parents, graph!.executed );
      
      if executed <> [ ] then
          
          t := VirtualCAS.task_name;
          
          wA := List( executed, i -> Concatenation( t, String( i ) ) );
          wA := JoinStringsWithSeparator( wA );
          
          v := stream!.variable_name;
          
          hv := List( executed, i -> Concatenation( "def ", v, String( i ), "=getResult(", t, String( i ), "); killTask(", t, String( i ), ");" ) );
          hv := Concatenation( hv );
          
          str := Concatenation(
                         "waitAllTasks(", wA, ");\n",
                         hv, "\n",
                         str
                         );

      fi;
      
      Append( executed, id );
      
  elif IsBlockingProcess( process ) and
    id = [ ] and parents <> [ ] then
      
      graph := stream.ProcessGraph;
      
      parents := Difference( parents, graph!.parentless );
      executed := Difference( parents, graph!.executed );
      
      if executed <> [ ] then
          
          t := VirtualCAS.task_name;
          
          wA := List( executed, i -> Concatenation( t, String( i ) ) );
          wA := JoinStringsWithSeparator( wA );
          
          v := stream!.variable_name;
          
          hv := List( executed, i -> Concatenation( "def ", v, String( i ), "=getResult(", t, String( i ), "); killTask(", t, String( i ), ");" ) );
          hv := Concatenation( hv );
          
          str := Concatenation(
                         "waitAllTasks(", wA, ");\n",
                         hv, "\n",
                         str
                         );
          
      fi;
      
  fi;
  
  Print( str );
  #Print( executed, "\n\n" );
  
  return [ str, executed ];
  
end;

##
InstallGlobalFunction( SendBlockingToVirtualCAS,
  function( arg )
    local stream, graph, process, processes, execute, p, str;
    
    if ( Length( arg ) = 2 and IsRecord( arg[1] ) and IsString( arg[2] ) ) then
        
        stream := arg[1];
        
        graph := stream.ProcessGraph;
        
        process := ProcessForHomalg( stream.description_of_last_process, arg[2] );
        
        Add( graph, process );
        
        if IsBlockingProcess( process ) then
            
            processes := graph!.processes;
            
            execute := processes{[ graph!.processed + 1 .. Length( processes ) ]};
            #Print( Length( execute ), "\n" );
            #ViewObj( execute ); Print( "\n\n" );
            
            
            for p in execute do
                
                str := CreateHPCString( stream, p );
                
                SendBlockingToCAS( stream, str[1] );
                
                Append( graph!.executed, str[2] );
                
            od;
            
            graph!.processed := Length( processes );
            
        fi;
        
    else
        Error( "wrong number or type of arguments\n" );
    fi;
    
end );

InstallGlobalFunction( TerminateVirtualCAS,
  function( arg )
    # Make this a no-op, as we can never re-start LibSingular
    # LibSingular will exit when gap exits
end );
