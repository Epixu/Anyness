///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Literal.hpp"
#include "Tuple.hpp"

namespace std
{
   template<class T>
   concept is_not_void = !is_void_v<T>;
}

namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Void<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Void = Yes<>;` in T                        
   template<class T>
   struct Void;

   /// Make sure no one interferes with true void type                        
   template<>
   struct Void<void> {};
   
   /// Can be used in two ways to satisfy CT::Typelist<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Typelist = Yes<>/No<>;` in T               
   template<class T>
   struct Typelist;
}

namespace Langulus::CT
{
   namespace Inner
   {
      ///   @note                                                             
      /// Concepts with ::std::decay_t<T>::CTTI_Void::Enabled bug out for     
      /// some reason. Probably because T may not be a user type, and         
      /// this isn't well handled as of yet by the compiler. I work around    
      /// this by using `if constexpr` to constrain the compiler further.     
      
      template<class T>
      consteval bool IsVoidInner() {
         using DT = ::std::remove_cvref_t<T>;
         if constexpr (Complete<CTTI::Void<DT>>)
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
         if constexpr (Complete<CTTI::Typelist<DT>>)
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
   /// It doesn't really carry any data, it's just a useful compile-time tool.
   /// Can be used to generate more complex types or tuples of data.          
   template<class...> struct Types;


   ///                                                                        
   /// An empty typelist.                                                     
   /// Satisfies CT::Void and is considered 'void'.                           
   template<>
   struct Types<> {
      using CTTI_Typelist = Yes<>;
      using CTTI_Void     = Yes<>;

      template<class...> friend struct Types;

      static constexpr bool   Empty = true;
      static constexpr size_t Count = 0;

      using First          = void;
      using Second         = void;
      using Tuple          = ::std::tuple<>;
      using TupleOptimized = compact_tuple<>;

      template<uint>
      using At = void;

      template<class>
      static constexpr bool Contains = false;

      consteval bool operator == (Types const&) const noexcept {
         return true;
      }
   };

   using NoTypes = Types<>;


   ///                                                                        
   /// Type list that contains exactly one type                               
   template<class T>
   struct Types<T> {
      using CTTI_Typelist = Yes<>;

      template<class...> friend struct Types;

      static constexpr bool   Empty = false;
      static constexpr size_t Count = 1;

      using First          = T;
      using Second         = void;
      using Tuple          = ::std::tuple<T>;
      using TupleOptimized = compact_tuple<T>;

      template<uint I>
      using At = Tif<I == 0, T, void>;

      template<class N>
      static constexpr bool Contains = ::std::same_as<N, T>;

      consteval bool operator == (Types const&) const noexcept {
         return true;
      }
   };


   ///                                                                        
   /// Type list that contains multiple types                                 
   template<class T1, class T2, class...TN>
   struct Types<T1, T2, TN...> {
      using CTTI_Typelist = Yes<>;

      template<class...> friend struct Types;

      static constexpr bool   Empty = false;
      static constexpr size_t Count = sizeof...(TN) + 2;

      using First          = T1;
      using Second         = T2;
      using Tuple          = ::std::tuple<T1, T2, TN...>;
      using TupleOptimized = compact_tuple<T1, T2, TN...>;

   private:
      template<uint I> LANGULUS(ALWAYS_INLINED)
      static consteval auto AtInner() {
         if      constexpr (I == 0)    return Types<T1> {};
         else if constexpr (I == 1)    return Types<T2> {};
         else if constexpr (I < Count) return Types<typename Types<TN...>::template At<I - 2>> {};
         else return Types<> {};
      }

   public:
      template<uint I>
      using At = typename decltype(AtInner<I>())::First;
   
      template<class N>
      static constexpr bool Contains =  ::std::same_as<N, T1>
                                    or  ::std::same_as<N, T2>
                                    or (::std::same_as<N, TN> or ...);

      consteval bool operator == (Types const&) const noexcept {
         return true;
      }
   };

   #define LglsTypegen(TYPES, LAMBDA) decltype(TYPES::GenerateTypes(LAMBDA));

   /// Retrieve the first type from a type list                               
   template<class...T>
   using FirstOf = typename Types<T...>::First;

   /// Retrieve the second type from a type list                              
   template<class...T>
   using SecondOf = typename Types<T...>::Second;
   
   template<class...LHS, class...RHS>
   consteval auto operator + (Types<LHS...>, Types<RHS...>) -> Types<LHS..., RHS...> {
      return {};
   }

   template<CT::Typelist...L>
   using ConcatenateTypeLists = decltype((LglsFake(L) + ...));

   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr void ForEach(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ (LglsLamb(lambda, T),...); },
            "Provided argument is not a lambda of the form []<class>");

         (LglsLamb(lambda, T), ...);
      }
   }

   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr bool ForEachAnd(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda, T),...)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");

         return (... and LglsLamb(lambda, T));
      }
      else return true; // 'AND' operator identity is true              
   }

   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr bool ForEachOr(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda, T),...)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");

         return (... or LglsLamb(lambda, T));
      }
      else return false; // 'OR' operator identity is false             
   }

   /// Doesn't generate code for further loops if lambda returns anything     
   /// but a No (utilizes a compile-time short-circuit)                       
   LANGULUS(ALWAYS_INLINED)
   constexpr No ForEachConstOr(Types<>, auto&&) { return {}; }

   template<class T1, class...TN> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) ForEachConstOr(Types<T1, TN...>, auto&& lambda) {
      static_assert(requires{ LglsLamb(lambda, T1); },
         "Lambda is not of the form []<class>");

      if constexpr (sizeof...(TN) == 0 or not ::std::same_as<No, decltype(LglsLamb(lambda, T1))>)
         return LglsLamb(lambda, T1);
      else {
         LglsLamb(lambda, T1);
         return ForEachConstOr(Types<TN...>{}, LglsFwd(lambda));
      }
   }

   template<uint IDX = 0, class...T> LANGULUS(ALWAYS_INLINED)
   constexpr void ForEachIndexed(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ (LglsLamb(lambda,T,0), ...); },
            "Provided argument is not a lambda of the form []<class,index>");

         [&]<uint...I>(::std::integer_sequence<uint, I...>&&) {
            (LglsLamb(lambda, T, IDX + I), ...);
         } (::std::make_integer_sequence<uint, sizeof...(T)>{});
      }
   }

   template<uint IDX = 0, class...T> LANGULUS(ALWAYS_INLINED)
   constexpr bool ForEachIndexedAnd(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda,T,0),...)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");

         return [&]<uint...I>(::std::integer_sequence<uint, I...>&&) {
            return (... and LglsLamb(lambda, T, IDX + I));
         } (::std::make_integer_sequence<uint, sizeof...(T)>{});
      }
      else return true;
   }

   template<uint IDX = 0, class...T> LANGULUS(ALWAYS_INLINED)
   constexpr bool ForEachIndexedOr(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda,T,0),...)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");

         return [&]<uint...I>(::std::integer_sequence<uint, I...>&&) {
            return (... or LglsLamb(lambda, T, IDX + I));
         } (::std::make_integer_sequence<uint, sizeof...(T)>{});
      }
      else return false;
   }
   
   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr auto Expand(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ LglsLamb(lambda, T...); },
            "Provided argument is not a lambda of the form []<class...>");

         return LglsLamb(lambda, T...);
      }
   }

   /// Generate a type list by providing a consteval generator lambda      
   ///   @param lambda the function that will generate the types.          
   ///          The lambda may or may not return Types, which will be      
   ///          concatenated along if so.                                  
   ///   @return a type list, containing the generated types               
   template<class...T> LANGULUS(ALWAYS_INLINED)
   consteval auto GenerateTypes(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda, T),...)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
            
         return Types<decltype(LglsLamb(lambda, T))...> {};
      }
      else return Types<>{};
   }

   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr auto GenerateData(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda, T),...)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");

         return ::std::tuple<T...> {LglsLamb(lambda, T)...};
      }
      else return ::std::tuple<>{};
   }

   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr auto GenerateDataOptimized(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ {(LglsLamb(lambda, T),...)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");

         return compact_tuple<T...> {LglsLamb(lambda, T)...};
      }
      else return compact_tuple<>{};
   }

   /// Discard elements for which lambda returns true                      
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Discard(Types<>, auto&&) { return Types<>{}; }

   template<class T1, class...TN> LANGULUS(ALWAYS_INLINED)
   constexpr auto Discard(Types<T1, TN...>, auto&& lambda) {
      static_assert(requires{ {LglsLambStatic(lambda, T1)} -> ::std::convertible_to<bool>; },
         "Provided argument is not a lambda of the form []<class> static -> convertible to bool");

      if constexpr (LglsLambStatic(lambda, T1)) {
         if constexpr (sizeof...(TN) > 0)
            return Discard(Types<TN...>{}, LglsFwd(lambda));
         else
            return Types<>{};
      }
      else {
         if constexpr (sizeof...(TN) > 0)
            return Types<T1>{} + Discard(Types<TN...>{}, LglsFwd(lambda));
         else
            return Types<T1>{};
      }
   }

   /// Collects stuff inside the types into a new value/type list          
   template<class...T> LANGULUS(ALWAYS_INLINED)
   constexpr auto Extract(Types<T...>, auto&& lambda) {
      if constexpr (sizeof...(T) != 0) {
         static_assert(requires{ (LglsLambStatic(lambda, T),...); },
            "Provided argument is not a lambda of the form []<class> static");
            
         return (... + LglsLambStatic(lambda, T));
      }
      else {
         static_assert(requires{ LglsLambStatic(lambda, void); },
            "Provided argument is not a lambda of the form []<class T> static where T can be 'void'"
            " - consider this case if you want to Extract from empty type lists");
         return decltype(LglsLambStatic(lambda, void)) {};
      }
   }

   LANGULUS(ALWAYS_INLINED)
   constexpr auto Reverse(Types<>) { return Types<>{}; }

   template<class T1, class...TN> LANGULUS(ALWAYS_INLINED)
   constexpr auto Reverse(Types<T1, TN...>) {
      if constexpr (sizeof...(TN) > 0)
         return Reverse(Types<TN...>{}) + Types<T1>{};
      else
         return Types<T1>{};
   }
}
