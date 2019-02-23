#############################################################################
##
##  VirtualCASBasic.gi                                    VirtualCAS package
##
##  Copyright 2011, Mohamed Barakat, University of Kaiserslautern
##                 Andreas Steenpaß, University of Kaiserslautern
##
##  Implementations for virtual rings.
##
#############################################################################

####################################
#
# global variables:
#
####################################

##
InstallValue( CommonHomalgTableForVirtualRingBasic,
        
        rec(
            
            ##  <#GAPDoc Label="BasisOfRowModule">
            ##  <ManSection>
            ##    <Func Arg="M" Name="BasisOfRowModule" Label="for virtual rings"/>
            ##    <Returns>a distinguished basis (i.e. a distinguished generating set) of the module generated by M</Returns>
            ##    <Description>
            ##      <Listing Type="Code"><![CDATA[
            BasisOfRowModule :=
              function( M )
                 return HomalgVirtualMatrix(
                                BasisOfRowModule( UnderlyingNonVirtualMatrix( M ) ),
                                HomalgRing( M ) );
              end,
            ##  ]]></Listing>
            ##    </Description>
            ##  </ManSection>
            ##  <#/GAPDoc>
            
            BasisOfColumnModule :=
              function( M )
                return HomalgVirtualMatrix( BasisOfColumnModule( UnderlyingNonVirtualMatrix( M ) ), HomalgRing( M ) );
              end,
            
            BasisOfRowsCoeff :=
              function( M, T )
                local S, TT, result;
                
                S := HomalgRing( M );
                
                TT := HomalgVoidMatrix( UnderlyingNonVirtualRing( S ) );
                
                result := BasisOfRowsCoeff( UnderlyingNonVirtualMatrix( M ), TT );
                
                SetEval( T, TT );
                
                return HomalgVirtualMatrix( result, S );
                
              end,
            
            BasisOfColumnsCoeff :=
              function( M, T )
                local S, TT, result;
                
                S := HomalgRing( M );
                
                TT := HomalgVoidMatrix( UnderlyingNonVirtualRing( S ) );
                
                result := BasisOfColumnsCoeff( UnderlyingNonVirtualMatrix( M ), TT );
                
                SetEval( T, TT );
                
                return HomalgVirtualMatrix( result, S );
                
              end,
            
            ##  <#GAPDoc Label="DecideZeroRows">
            ##  <ManSection>
            ##    <Func Arg="A, B" Name="DecideZeroRows" Label="for virtual rings"/>
            ##    <Returns>a reduced form of <A>A</A> with respect to <A>B</A></Returns>
            ##    <Description>
            ##      <Listing Type="Code"><![CDATA[
            DecideZeroRows :=
              function( A, B )
                return HomalgVirtualMatrix(
                               DecideZeroRows( UnderlyingNonVirtualMatrix( A ),
                                       UnderlyingNonVirtualMatrix( B ) ),
                               HomalgRing( A ) );
              end,
            ##  ]]></Listing>
            ##    </Description>
            ##  </ManSection>
            ##  <#/GAPDoc>
            
            DecideZeroColumns :=
              function( A, B )
                return HomalgVirtualMatrix( DecideZeroColumns( UnderlyingNonVirtualMatrix( A ), UnderlyingNonVirtualMatrix( B ) ), HomalgRing( A ) );
              end,
            
            DecideZeroRowsEffectively :=
              function( A, B, T )
                local S, TT, result;
                
                S := HomalgRing( A );
                
                TT := HomalgVoidMatrix( UnderlyingNonVirtualRing( S ) );
                
                result := DecideZeroRowsEffectively( UnderlyingNonVirtualMatrix( A ), UnderlyingNonVirtualMatrix( B ), TT );
                
                SetEval( T, TT );
                
                return HomalgVirtualMatrix( result, S );
                
              end,
            
            DecideZeroColumnsEffectively :=
              function( A, B, T )
                local S, TT, result;
                
                S := HomalgRing( A );
                
                TT := HomalgVoidMatrix( UnderlyingNonVirtualRing( S ) );
                
                result := DecideZeroColumnsEffectively( UnderlyingNonVirtualMatrix( A ), UnderlyingNonVirtualMatrix( B ), TT );
                
                SetEval( T, TT );
                
                return HomalgVirtualMatrix( result, S );
                
              end,
            
            ##  <#GAPDoc Label="SyzygiesGeneratorsOfRows">
            ##  <ManSection>
            ##    <Func Arg="M" Name="SyzygiesGeneratorsOfRows" Label="for virtual rings"/>
            ##    <Returns>a distinguished basis of the syzygies of the argument</Returns>
            ##    <Description>
            ##      <Listing Type="Code"><![CDATA[
            SyzygiesGeneratorsOfRows :=
              function( M )
                return HomalgVirtualMatrix(
                               SyzygiesGeneratorsOfRows( UnderlyingNonVirtualMatrix( M ) ),
                               HomalgRing( M ) );
              end,
            ##  ]]></Listing>
            ##    </Description>
            ##  </ManSection>
            ##  <#/GAPDoc>
            
            RelativeSyzygiesGeneratorsOfRows :=
              function( M, N )
                return HomalgVirtualMatrix( SyzygiesGeneratorsOfRows( UnderlyingNonVirtualMatrix( M ), UnderlyingNonVirtualMatrix( N ) ), HomalgRing( M ) );
              end,
            
            SyzygiesGeneratorsOfColumns :=
              function( M )
                return HomalgVirtualMatrix(
                               SyzygiesGeneratorsOfColumns( UnderlyingNonVirtualMatrix( M ) ),
                               HomalgRing( M ) );
              end,
            
            RelativeSyzygiesGeneratorsOfColumns :=
              function( M, N )
                return HomalgVirtualMatrix( SyzygiesGeneratorsOfColumns( UnderlyingNonVirtualMatrix( M ), UnderlyingNonVirtualMatrix( N ) ), HomalgRing( M ) );
              end,
     )
  );
