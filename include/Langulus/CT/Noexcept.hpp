///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Core.hpp"


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

      template<class F>
      auto IsNoexcept(F) -> decltype(IsNoexcept(&F::operator())) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }
   }

   /// Check if any kind of function F is noexcept                            
   template<class F>
   static constexpr bool IsNoexcept =
      ::std::same_as<decltype(Inner::IsNoexcept(Fake<F>())), ::std::true_type>;
}

/// Convenience macro for propagating noexceptness                            
#define noexcept_if(LAMBDA) noexcept(::Langulus::IsNoexcept<decltype(LAMBDA)>)
