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
      using Reverse        = Types<>;
      using Tuple          = ::std::tuple<>;
      using TupleOptimized = compact_tuple<>;

      static constexpr void ForEach          (auto&&) {}
      static consteval bool ForEachAnd       (auto&&) { return false; }
      static consteval bool ForEachOr        (auto&&) { return false; }
      template<uint = 0>
      static constexpr void ForEachIndexed   (auto&&) {}
      template<uint = 0>
      static consteval bool ForEachIndexedAnd(auto&&) { return false; }
      template<uint = 0>
      static consteval bool ForEachIndexedOr (auto&&) { return false; }
      static consteval No   ForEachConstOr   (auto&&) { return {}; }
      static constexpr void Expand           (auto&&) {}

      template<uint>
      using At = void;

      static consteval auto GenerateTypes(auto&&) { return Types<> {}; }
      static consteval auto GenerateData(auto&&) { return Tuple {}; }
      static consteval auto GenerateDataOptimized(auto&&) { return TupleOptimized {}; }
      static constexpr auto Discard(auto&&) { return Types<>{}; }
      static constexpr auto Extract(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, void); },
            "Provided argument is not a lambda of the form []<class T> where T can be 'void'"
            " - consider this case if you want to Extract from empty type lists");
         return decltype(LglsLamb(lambda, void)) {};
      }

      template<class...N>
      consteval auto operator + (Types<N...>&&) const -> Types<N...> { return {}; }

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
      using Reverse        = Types<T>;
      using Tuple          = ::std::tuple<T>;
      using TupleOptimized = compact_tuple<T>;

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda,T); },
            "Provided argument is not a lambda of the form []<class>");
         LglsLamb(lambda, T);
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return LglsLamb(lambda, T);
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return LglsLamb(lambda, T);
      }

      template<uint IDX = 0>
      static constexpr void ForEachIndexed(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda,T,0); },
            "Provided argument is not a lambda of the form []<class,index>");
         LglsLamb(lambda, T, IDX);
      }

      template<uint IDX = 0>
      static constexpr bool ForEachIndexedAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T,0)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         return LglsLamb(lambda, T, IDX);
      }

      template<uint IDX = 0>
      static constexpr bool ForEachIndexedOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T,0)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         return LglsLamb(lambda, T, IDX);
      }

      /// Just executes lambda with the contained type and returns its result 
      static constexpr decltype(auto) ForEachConstOr(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T); },
            "Provided argument is not a lambda of the form []<class>");
         return LglsLamb(lambda, T);
      }

      static constexpr auto Expand(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T); },
            "Provided argument is not a lambda of the form []<class...>");
         return LglsLamb(lambda, T);
      }

      template<uint I>
      using At = Tif<I == 0, T, void>;

      /// Generate a type list by providing a consteval generator lambda      
      ///   @param lambda the function that will generate the types           
      ///      the lambda may or may not return a type list, which will be    
      ///      concatenated along if so                                       
      ///   @return a type list, containing the generated types               
      static consteval auto GenerateTypes(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return Types<decltype(LglsLamb(lambda, T))> {};
      }

      static constexpr auto GenerateData(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return Tuple {LglsLamb(lambda, T)};
      }

      static constexpr auto GenerateDataOptimized(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return TupleOptimized {LglsLamb(lambda, T)};
      }

      /// Discard elements for which lambda returns true                      
      static constexpr auto Discard(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         if constexpr (LglsLamb(lambda, T))
            return Types<>{};
         else
            return Types<T>{};
      }

      /// Collects stuff inside the types into a new value/type list          
      static constexpr auto Extract(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T); },
            "Provided argument is not a lambda of the form []<class>");
         return LglsLamb(lambda, T);
      }

      template<class...N>
      consteval auto operator + (Types<N...>&&) const -> Types<T, N...> { return {}; }

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
      using Reverse        = decltype(LglsFake(typename Types<TN...>::Reverse).operator + (LglsFake(Types<T2, T1>)));
      using Tuple          = ::std::tuple<T1, T2, TN...>;
      using TupleOptimized = compact_tuple<T1, T2, TN...>;

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T1); },
            "Provided argument is not a lambda of the form []<class>");
          LglsLamb(lambda, T1);
          LglsLamb(lambda, T2);
         (LglsLamb(lambda, TN), ...);
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return LglsLamb(lambda, T1)
            and LglsLamb(lambda, T2)
            and (... and LglsLamb(lambda, TN));
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         return LglsLamb(lambda, T1)
             or LglsLamb(lambda, T2)
             or (... or LglsLamb(lambda, TN));
      }

      /// Doesn't generate code for further loops if lambda returns anything  
      /// but a No (utilizes a compile-time short-circuit)                    
      static constexpr decltype(auto) ForEachConstOr(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T1); },
            "Lambda is not of the form []<class>");
         if constexpr (not ::std::same_as<No, decltype(LglsLamb(lambda, T1))>)
            return LglsLamb(lambda, T1);
         else {
            LglsLamb(lambda, T1);
            static_assert(requires{ LglsLamb(lambda, T2); },
               "Lambda is not of the form []<class>");
            if constexpr (not ::std::same_as<No, decltype(LglsLamb(lambda, T2))>)
               return LglsLamb(lambda, T2);
            else {
               LglsLamb(lambda, T2);
               return Types<TN...>::ForEachConstOr(LglsFwd(lambda));
            }
         }
      }

      template<uint IDX = 0>
      static constexpr void ForEachIndexed(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda,T1,0); },
            "Provided argument is not a lambda of the form []<class,index>");
         LglsLamb(lambda, T1, IDX + 0);
         LglsLamb(lambda, T2, IDX + 1);
         if constexpr (sizeof...(TN) > 0)
            Types<TN...>::template ForEachIndexed<IDX + 2>(LglsFwd(lambda));
      }

      template<uint IDX = 0>
      static constexpr bool ForEachIndexedAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T1,0)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         if constexpr (sizeof...(TN) > 0) {
            return LglsLamb(lambda, T1, IDX + 0)
               and LglsLamb(lambda, T2, IDX + 1)
               and Types<TN...>::template ForEachIndexedAnd<IDX + 2>(LglsFwd(lambda));
         }
         else {
            return LglsLamb(lambda, T1, IDX + 0)
               and LglsLamb(lambda, T2, IDX + 1);
         }
      }

      template<uint IDX = 0>
      static constexpr bool ForEachIndexedOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T1,0)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class,index> -> convertible to bool");
         if constexpr (sizeof...(TN) > 0) {
            return LglsLamb(lambda, T1, IDX + 0)
                or LglsLamb(lambda, T2, IDX + 1)
                or Types<TN...>::template ForEachIndexedOr<IDX + 2>(LglsFwd(lambda));
         }
         else {
            return LglsLamb(lambda, T1, IDX + 0)
                or LglsLamb(lambda, T2, IDX + 1);
         }
      }
      
      static constexpr auto Expand(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T1, T2, TN...); },
            "Provided argument is not a lambda of the form []<class...>");
         return LglsLamb(lambda, T1, T2, TN...);
      }

   private:
      template<uint I>
      static consteval auto AtInner() {
              if constexpr (I == 0)    return Types<T1> {};
         else if constexpr (I == 1)    return Types<T2> {};
         else if constexpr (I < Count) return Types<typename Types<TN...>::template At<I - 2>> {};
         else return Types<> {};
      }

   public:
      template<uint I>
      using At = typename decltype(AtInner<I>())::First;

      /// Generate a type list by providing a consteval generator lambda      
      ///   @param lambda the function that will generate the types.          
      ///          The lambda may or may not return Types, which will be      
      ///          concatenated along if so.                                  
      ///   @return a type list, containing the generated types               
      static consteval auto GenerateTypes(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda,T1)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return Types<
            decltype(LglsLamb(lambda, T1)),
            decltype(LglsLamb(lambda, T2)),
            decltype(LglsLamb(lambda, TN))...
         > {};
      }

      static constexpr auto GenerateData(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T1)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return Tuple {
            LglsLamb(lambda, T1),
            LglsLamb(lambda, T2),
            LglsLamb(lambda, TN)...
         };
      }

      static constexpr auto GenerateDataOptimized(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T1)} -> CT::NotVoid; },
            "Provided argument is not a lambda of the form []<class> -> non-void type");
         return TupleOptimized {
            LglsLamb(lambda, T1),
            LglsLamb(lambda, T2),
            LglsLamb(lambda, TN)...
         };
      }

      /// Discard elements for which lambda returns true                      
      static constexpr auto Discard(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, T1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<class> -> convertible to bool");
         if constexpr (LglsLamb(lambda, T1)) {
            if constexpr (LglsLamb(lambda, T2)) {
               if constexpr (sizeof...(TN) > 0)
                  return Types<TN...>::Discard(LglsFwd(lambda));
               else
                  return Types<>{};
            }
            else {
               if constexpr (sizeof...(TN) > 0)
                  return Types<T2>{} + Types<TN...>::Discard(LglsFwd(lambda));
               else
                  return Types<T2>{};
            }
         }
         else {
            if constexpr (LglsLamb(lambda, T2)) {
               if constexpr (sizeof...(TN) > 0)
                  return Types<T1>{} + Types<TN...>::Discard(LglsFwd(lambda));
               else
                  return Types<T1>{};
            }
            else {
               if constexpr (sizeof...(TN) > 0)
                  return Types<T1, T2>{} + Types<TN...>::Discard(LglsFwd(lambda));
               else
                  return Types<T1, T2>{};
            }
         }
      }

      /// Collects stuff inside the types into a new value/type list          
      static constexpr auto Extract(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, T1); },
            "Provided argument is not a lambda of the form []<class>");
         if constexpr (sizeof...(TN) > 0) {
            return LglsLamb(lambda, T1)
                +  LglsLamb(lambda, T2)
                + (LglsLamb(lambda, TN) + ...);
         }
         else return LglsLamb(lambda, T1) + LglsLamb(lambda, T2);
      }

      template<class...N>
      consteval auto operator + (Types<N...>&&) const -> Types<T1, T2, TN..., N...> { return {}; }

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

   template<CT::Typelist...L>
   using ConcatenateTypeLists = decltype((LglsFake(L) + ...));
}
