///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Literal.hpp"
#include <tuple>


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Void<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Void = Yes<>/No<>;` in T                   
   template<class T>
   struct Void {
      static constexpr bool Enabled = false;
   };

   /// Make sure no one interferes with true void types                       
   template<>
   struct Void<void> {
      static constexpr bool Enabled = true;
   };

   
   /// Can be used in two ways to satisfy CT::Typelist<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Typelist = Yes<>/No<>;` in T               
   template<class T>
   struct Typelist {
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT
{
   namespace Inner
   {
      ///   @note                                                             
      /// Concepts with ::std::decay_t<T>::CTTI_Void::Enabled bug out for     
      /// some reason. Probably because T may not be a user type, and         
      /// this isn't well handled as of yet by the compiler. I work around    
      /// this by using `if constexpr` to constrain the compiler further      
      
      template<class T>
      consteval bool IsVoidInner() {
         using DT = ::std::remove_cvref_t<T>;

         if constexpr (CTTI::Void<DT>::Enabled)
            return true;
         else if constexpr (::std::is_class_v<DT>) {
            static_assert(Complete<DT>,
               "Can't check if an incomplete type is void");
            // Access member only if T is an user type, to save the     
            // compiler from bugging out                                
            if constexpr (requires { DT::CTTI_Void::Enabled; })
               return DT::CTTI_Void::Enabled;
            else
               return false;
         }
         else return false;
      }

      template<class T>
      consteval bool IsTypelistInner() {
         using DT = ::std::remove_cvref_t<T>;

         if constexpr (CTTI::Typelist<DT>::Enabled)
            return true;
         else if constexpr (::std::is_class_v<DT>) {
            static_assert(Complete<DT>,
               "Can't check if an incomplete type is a type list");
            // Access member only if T is an user type, to save the     
            // compiler from bugging out                                
            if constexpr (requires { DT::CTTI_Typelist::Enabled; })
               return DT::CTTI_Typelist::Enabled;
            else
               return false;
         }
         else return false;
      }
   }

   /// Check if all T are marked void                                         
   template<class...T>
   concept Void = PartialValidate<T...>
       and (Inner::IsVoidInner<T>() and ...);

   template<class...T>
   concept NotVoid = PartialValidate<T...>
       and ((not Inner::IsVoidInner<T>()) and ...);

   /// Check if all T are typelists                                           
   template<class...T>
   concept Typelist = PartialValidate<T...>
       and (Inner::IsTypelistInner<T>() and ...);

   template<class...T>
   concept NotTypelist = PartialValidate<T...>
       and ((not Inner::IsTypelistInner<T>()) and ...);
}

namespace Langulus
{

   ///                                                                        
   ///   Compile-time type list                                               
   ///                                                                        
   /// It doesn't really carry any data, it's just a useful compile-time tool 
   /// Can be used to generate more complex types or tuples of data           
   ///                                                                        
   template<class...T>
   struct Types;

   namespace Inner
   {
      template<CT::Typelist GATHERED, class HEAD, class...TAIL>
      static consteval CT::Typelist auto GenerateTypes(auto&& lambda) {
         using R = decltype(lambda.template operator()<HEAD>());
         using C = typename GATHERED::template Concat<R>;
         if constexpr (sizeof...(TAIL))
            return GenerateTypes<C, TAIL...>(lambda);
         else
            return C {};
      }
   }


   ///                                                                        
   /// An empty typelist                                                      
   /// Satisfies CT::Void and is considered 'void'                            
   template<>
   struct Types<> {
      using CTTI_Typelist = Yes<>;
      using CTTI_Void     = Yes<>;

      static constexpr bool   Empty = true;
      static constexpr size_t Count = 0;
      using First = void;

      template<class...N>
      // ReSharper disable once CppFunctionIsNotImplemented             
      static consteval auto Concat(Types<N...>&&) -> Types<N...>;
      // ReSharper disable once CppFunctionIsNotImplemented             
      template<class N>
      static consteval auto Concat(N&&) -> Types<N>;

      template<class N>
      using Cat = decltype(Concat(Fake<N&&>()));
   };

   using NoTypes = Types<>;


   ///                                                                        
   /// Type list that contains exactly one type                               
   template<class T>
   struct Types<T> {
      using CTTI_Typelist = Yes<>;

      static constexpr bool Empty = false;
      static constexpr size_t Count = 1;
      using First = T;

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<T>(); },
            "Provided argument is not a lambda of the form []<class>");
         lambda.template operator()<T>();
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return lambda.template operator()<T>();
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return lambda.template operator()<T>();
      }

      static constexpr void ForEachIndexed(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<T,0>(); },
            "Provided argument is not a lambda of the form []<class,index>");
         lambda.template operator()<T,0>();
      }

      static constexpr bool ForEachIndexedAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T,0>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         return lambda.template operator()<T,0>();
      }

      static constexpr bool ForEachIndexedOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T,0>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         return lambda.template operator()<T,0>();
      }
      
      static constexpr auto Expand(auto&& lambda) {
         return ForEach(FWD(lambda));
      }

      template<unsigned I>
      static consteval auto AtInner() {
         static_assert(I == 0, "Index is out of type list bounds (list has one type)");
         return Types<T> {};
      }

      template<unsigned I>
      using At = typename decltype(AtInner<I>())::First;

      /// Generate a type list by providing a consteval generator lambda      
      ///   @param lambda - the function that will generate the types         
      ///      the lambda may or may not return a type list, which will be    
      ///      concatenated along if so                                       
      ///   @return a type list, containing the generated types               
      // ReSharper disable once CppEntityUsedOnlyInUnevaluatedContext   
      static consteval CT::Typelist auto GenerateTypes(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T>()} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         using R = decltype(lambda.template operator()<T>());
         if constexpr (CT::Typelist<R>)
            return R {};
         else
            return Types<R> {};
      }

      using Tuple = ::std::tuple<T>;

      static constexpr Tuple GenerateData(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T>()} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return {lambda.template operator()<T>()};
      }

      template<class...N>
      // ReSharper disable once CppFunctionIsNotImplemented             
      static consteval auto Concat(Types<N...>&&) -> Types<T, N...>;
      template<class N>
      // ReSharper disable once CppFunctionIsNotImplemented             
      static consteval auto Concat(N&&) -> Types<T, N>;

      template<class N>
      using Cat = decltype(Concat(Fake<N&&>()));

      template<class N>
      static constexpr bool Contains = ::std::same_as<N, T>;
   };


   ///                                                                        
   /// Type list that contains multiple types                                 
   template<class T1, class T2, class...TN>
   struct Types<T1, T2, TN...> {
      using CTTI_Typelist = Yes<>;

      static constexpr bool Empty = false;
      static constexpr size_t Count = sizeof...(TN) + 2;
      using First = T1;
      using Second = T2;

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<T1>(); },
            "Provided argument is not a lambda of the form []<class>");
          lambda.template operator()<T1>();
          lambda.template operator()<T2>();
         (lambda.template operator()<TN>(), ...);
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return lambda.template operator()<T1>()
            and lambda.template operator()<T2>()
            and (... and lambda.template operator()<TN>());
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return lambda.template operator()<T1>()
             or lambda.template operator()<T2>()
             or (... or lambda.template operator()<TN>());
      }

      /// Doesn't generate code for further loops if lambda returns           
      /// Yes instead of No                                                   
      /// (utilizes a compile-time short-circuit)                             
      static constexpr bool ForEachConstOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1>()} -> ::std::same_as<Yes<>>;  }
                    or requires{ {lambda.template operator()<T1>()} -> ::std::same_as<No>; },
            "Provided argument is not a lambda of the form []<class> -> Yes<>/No<>");
         if constexpr (::std::same_as<Yes<>, decltype(lambda.template operator()<T1>())>) {
            lambda.template operator()<T1>();
            return true;
         }
         else if constexpr (::std::same_as<Yes<>, decltype(lambda.template operator()<T2>())>) {
            lambda.template operator()<T2>();
            return true;
         }
         else if constexpr (sizeof...(TN))
            return Types<TN...>::ForEachConstOr(lambda);
         else
            return false;
      }

      template<unsigned IDX = 0>
      static constexpr void ForEachIndexed(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<T1,0>(); },
            "Provided argument is not a lambda of the form []<class,index>");
          lambda.template operator()<T1, IDX + 0>();

          if constexpr (Count > 1) {
             lambda.template operator()<T2, IDX + 1>();

             if constexpr (sizeof...(TN) > 0)
               Types<TN...>::template ForEachIndexed<IDX + 2>(lambda);
          }
      }

      template<unsigned IDX = 0>
      static constexpr bool ForEachIndexedAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1,0>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         if constexpr (Count == 1)
            return lambda.template operator()<T1, IDX>();
         else if constexpr (Count == 2)
            return lambda.template operator()<T1, IDX + 0>()
               and lambda.template operator()<T2, IDX + 1>();
         else if constexpr (Count > 2)
            return lambda.template operator()<T1, IDX + 0>()
               and lambda.template operator()<T2, IDX + 1>()
               and Types<TN...>::template ForEachIndexedAnd<IDX + 2>(lambda);
         else return false;
      }

      template<unsigned IDX = 0>
      static constexpr bool ForEachIndexedOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1,0>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         if constexpr (Count == 1)
            return lambda.template operator()<T1, IDX>();
         else if constexpr (Count == 2)
            return lambda.template operator()<T1, IDX + 0>()
                or lambda.template operator()<T2, IDX + 1>();
         else if constexpr (Count > 2)
            return lambda.template operator()<T1, IDX + 0>()
                or lambda.template operator()<T2, IDX + 1>()
                or Types<TN...>::template ForEachIndexedOr<IDX + 2>(lambda);
         else return false;
      }
      
      static constexpr auto Expand(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<T1, T2, TN...>(); },
            "Provided argument is not a lambda of the form []<class...>");
         return lambda.template operator()<T1, T2, TN...>();
      }

      template<unsigned I>
      static consteval auto AtInner() {
         if constexpr (I == 0)
            return Types<T1> {};
         else if constexpr (I == 1)
            return Types<T2> {};
         else if constexpr (I < Count)
            return Types<TN...>::template AtInner<I - 2>();
         else
            static_assert(false, "Index is out of type list bounds");
      }

      template<unsigned I>
      using At = typename decltype(AtInner<I>())::First;

      /// Generate a type list by providing a consteval generator lambda      
      ///   @param lambda - the function that will generate the types         
      ///          the lambda may or may not return Types, which will be      
      ///          concatenated along if so                                   
      ///   @return a type list, containing the generated types               
      static consteval CT::Typelist auto GenerateTypes(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1>()} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return Inner::GenerateTypes<NoTypes, T1, T2, TN...>(lambda);
      }

      using Tuple = ::std::tuple<T1, T2, TN...>;

      static constexpr Tuple GenerateData(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<T1>()} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return {
            lambda.template operator()<T1>(),
            lambda.template operator()<T2>(),
            lambda.template operator()<TN>()...
         };
      }

      template<class...N>
      // ReSharper disable once CppFunctionIsNotImplemented             
      static consteval auto Concat(Types<N...>&&) -> Types<T1, T2, TN..., N...>;
      template<class N>
      // ReSharper disable once CppFunctionIsNotImplemented             
      static consteval auto Concat(N&&) -> Types<T1, T2, TN..., N>;

      template<class N>
      using Cat = decltype(Concat(Fake<N&&>()));

      template<class N>
      static constexpr bool Contains = ForEachConstOr([]<class A> {
         if constexpr (::std::same_as<N, A>)
            return Yes {};
         else
            return No {};
      });
   };

   #define LglsTypegen(TYPES, LAMBDA) decltype(TYPES::GenerateTypes(LAMBDA));

   /// Retrieve the first type from a type list                               
   template<class...T>
   using FirstOf = typename Types<T...>::First;

   /// Retrieve the second type from a type list                              
   template<class...T>
   using SecondOf = typename Types<T...>::Second;
}
