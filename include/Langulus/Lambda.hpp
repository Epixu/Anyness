#pragma once
#include "Types.hpp"


namespace Langulus
{
   
   namespace Inner
   {

      /// These function declarations are used to decompose lambdas           
      /// You can use it to extract the argument types of functions, by using 
      /// decltype on the function return.                                    
      /// Handles functors, member/standing function pointers, lambdas.       
      template<class R, class F, class A, class...AN>
      Types<A, AN...> GetFunctionArguments(R(F::*)(A, AN...) const) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class R, class F, class A, class...AN>
      Types<A, AN...> GetFunctionArguments(R(F::*)(A, AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class R, class A, class...AN>
      Types<A, AN...> GetFunctionArguments(R(*)(A, AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class F>
      decltype(GetFunctionArguments(&F::operator())) GetFunctionArguments(F) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

   } // namespace Langulus::Inner

   /// Get the type of the first argument of a function                       
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentOf = typename decltype(Inner::GetFunctionArguments(Fake<F>()))::First;

   /// Get a type list corresponding to the function arguments                
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentsOf = decltype(Inner::GetFunctionArguments(Fake<F>()));

   /// Get the return type of a function                                      
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ReturnOf = decltype((Fake<F>()) (Fake<ArgumentOf<F>>()));

   /// Check if a lambda call is noexcept                                     
   template<class F>
   static constexpr bool IsNoexcept = not LANGULUS_SAFE()
      and noexcept(Fake<F&&>().operator() (Fake<ArgumentOf<F>>()));

} // namespace Langulus
