///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Lambda.hpp"


namespace Langulus
{
   namespace Inner
   {
      template<class R, class F, class...AN>
      auto IsNoexcept(R(F::*)(AN...) const) -> ::std::false_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      auto IsNoexcept(R(F::*)(AN...) const noexcept) -> ::std::true_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      auto IsNoexcept(R(F::*)(AN...)) -> ::std::false_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      auto IsNoexcept(R(F::*)(AN...) noexcept) -> ::std::true_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class...AN>
      auto IsNoexcept(R(*)(AN...)) -> ::std::false_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class...AN>
      auto IsNoexcept(R(*)(AN...) noexcept) -> ::std::true_type {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }
      

      ///                                                                     
      /// The following overrides peek inside lambdas/functors and test their 
      /// operator() instead. You might have to specialize these for your     
      /// template signatures, as they can't be generalized here yet          
      /// Make sure you notify me if you figure out a general solution        
      ///                                                                     
      
      /// Used for non-templated lambdas                                      
      template<class F>
      auto IsNoexcept(F&&) -> decltype(IsNoexcept(decltype(&F::operator()) {})) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }
      
      /// This particular override is used in Sequence.hpp                    
      template<class F>
      auto IsNoexcept(F&&) -> decltype(IsNoexcept(decltype(&F::template operator()<0>) {})) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }
   }

   /// True if all functions F... are noexcept                                
   template<class...F>
   static constexpr bool IsNoexcept = CT::PartialValidate<F...> and (
         ::std::same_as<decltype(Inner::IsNoexcept(LglsFake(F&&))), ::std::true_type>
      and ...);
}

/// Convenience macro for propagating noexceptness                            
#define noexcept_if(LAMBDA) noexcept(::Langulus::IsNoexcept<decltype(LAMBDA)>)
