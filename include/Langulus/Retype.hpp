///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"


namespace Langulus
{
   namespace Inner
   {

      /// Non-templated retyping (relies on `typename T::template Retype`)    
      template<class T>
      struct RetypeInner {
         template<class A1, class...AN>
         static constexpr bool HasCustomRetyper = requires {
            typename T::template Retype<A1, AN...>;
         };

         template<class A1, class...AN>
         using Retype = Tif<HasCustomRetyper<A1, AN...>,
            typename T::template Retype<A1, AN...>,
            T
         >;
      };

      /// Templated retyping                                                  
      template<template<class> class ORIGINAL, class OLD_ARG>
      struct RetypeInner<ORIGINAL<OLD_ARG>> {
         using T = ORIGINAL<OLD_ARG>;

         template<class NEW_ARG>
         static constexpr bool HasCustomRetyper = requires {
            typename T::template Retype<NEW_ARG>;
         };

         template<class NEW_ARG>
         using Retype = Tif<HasCustomRetyper<NEW_ARG>,
            typename T::template Retype<NEW_ARG>,
            ORIGINAL<NEW_ARG>
         >;
      };

   } // namespace Langulus::Inner

   /// This retype tool will take a (templated or not) T, and subsitute its   
   /// type with another. If `typename T::template Retype` exists, it will be 
   /// used instead. If T is neither templated, nor has a Retype member, then 
   /// T remains unchanged                                                    
   template<class T, class...ARGUMENTS>
   using Retype = typename Inner::RetypeInner<T>::template Retype<ARGUMENTS...>;

} // namespace Langulus
