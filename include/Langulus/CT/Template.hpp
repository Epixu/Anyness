///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Types.hpp"


namespace Langulus::CT::Inner
{
   template<class>
   struct IsTemplate {
      static constexpr bool Value = false;
      static constexpr size_t ArgumentCount = 0;
      using Arguments = void;
   };

   template<template<class...> class T, class...ARGS>
   struct IsTemplate<T<ARGS...>> {
      static constexpr bool Value = true;
      static constexpr size_t ArgumentCount = sizeof...(ARGS);
      using Arguments = Types<ARGS...>;

      template<class...NEW_ARGS>
      using Retype = T<NEW_ARGS...>;
   };
}

namespace Langulus::CT
{
   /// Tests whether all T are templated types                                
   template<class...T>
   concept Template = PartialValidate<T...>
       and (Inner::IsTemplate<T>::Value and ...);

   /// Get the type list with all template arguments                          
   template<class T>
   using TemplateArgumentsOf = typename Inner::IsTemplate<T>::Arguments;
}
