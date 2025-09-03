///                                                                           
/// Langulus                                                                  
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Types.hpp"


namespace Langulus
{
   namespace Inner
   {
      ///                                                                     
      /// Some tools extracted from https://github.com/ZigaSajovic/CppML      
      ///                                                                     
      namespace ml
      {
         template<class T, T t>
         struct Value {
            using type = T;
            static constexpr T value = t;
         };

         template <int  N> using Int  = Value<int,  N>;
         template <bool N> using Bool = Value<bool, N>;
         //template <char C> using Char = Value<char, C>;

         /// Used in tag-dispatch patterns                                    
         struct _ {};
         
         /// Represents a "nothing" type                                      
         struct None {};

         /// Returns self                                                     
         struct Identity {
            template<class T> using f = T;
         };

         /// Invokes a meta function                                          
         template<class Pipe, class...Ts>
         using f = typename Pipe::template f<Ts...>;

         namespace Implementations
         {
            template<bool>
            struct IfElse;

            template<>
            struct IfElse<true>  { template<class T, class U> using f = T; };

            template<>
            struct IfElse<false> { template<class T, class U> using f = U; };

            template<bool>
            struct DelayTemplateEval {
               template<template<class...> class F0, class...Ts>
               using f = F0<Ts...>;
            };

            template<>
            struct DelayTemplateEval<false> {
               template<template<class...> class F0, class...Ts>
               using f = F0<>;
            };
         }

         /// Delays a call, so compiler knows the number of arguments         
         template<class F, int N>
         using DelayedEval = typename Implementations::IfElse<(N < 10000)>::template f<F, void>;

         /// Delays an alias call, so compiler knows the number of arguments  
         template<template<class...> class F, int N, class...Ts>
         using DelayedAliasEval = typename Implementations::DelayTemplateEval<(
            N < 100000)>::template f<F, Ts...>;

         /// Creates a metafunction out of a template                         
         template<template<class...> class F_, class Pipe = Identity>
         struct F {
            template <class...Args>
            using f = ml::f<Pipe, ml::DelayedAliasEval<F_, sizeof...(Args), Args...>>;
         };


         namespace Implementations
         {
            namespace _Pivot
            {
               struct GetNext {
                  template<int i>
                  using f = Int<i >= 32
                     ? 32 : i >= 16 ? 16 : i >= 8 ? 8 : i >= 4 ? 4 : i >= 2 ? 2 : i >= 1 ? 1 : 0
                  >;
               };
            }

            template<int i>
            struct Pivot;

            template<>
            struct Pivot<0> {
               template<class Pipe, class N, class...Ts>
               using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>, Ts...>;
            };

            template<>
            struct Pivot<1> {
               template<class Pipe, class N, class T0, class...Ts>
               using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>, Ts..., T0>;
            };

            template<>
            struct Pivot<2> {
               template <class Pipe, class N, class T0, class T1, class...Ts>
               using f = ml::f<DelayedEval<Pivot<_Pivot::GetNext::f<N::value - 2>::value>,
                  sizeof...(Ts)>,
                  Pipe, Int<N::value - 2>, Ts..., T0, T1
               >;
            };

            template<>
            struct Pivot<4> {
               template <class Pipe, class N, class T0, class T1, class T2, class T3, class... Ts>
               using f = ml::f<DelayedEval<Pivot<_Pivot::GetNext::f<N::value - 4>::value>,
                  sizeof...(Ts)>,
                  Pipe, Int<N::value - 4>, Ts..., T0, T1, T2, T3
               >;
            };

            template<>
            struct Pivot<8> {
               template <class Pipe, class N, class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class...Ts>
               using f = ml::f<DelayedEval<Pivot<_Pivot::GetNext::f<N::value - 8>::value>,
                  sizeof...(Ts)>,
                  Pipe, Int<N::value - 8>, Ts..., T0, T1, T2, T3, T4, T5, T6, T7
               >;
            };

            template<>
            struct Pivot<16> {
               template <class Pipe, class N, class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9, class T10, class T11, class T12, class T13, class T14, class T15, class... Ts>
               using f = ml::f<DelayedEval<Pivot<_Pivot::GetNext::f<N::value - 16>::value>,
                  sizeof...(Ts)>,
                  Pipe, Int<N::value - 16>, Ts..., T0, T1, T2, T3, T4, T5, T6, T7,
                  T8, T9, T10, T11, T12, T13, T14, T15
               >;
            };

            template<>
            struct Pivot<32> {
               template <class Pipe, class N, class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9, class T10, class T11, class T12, class T13, class T14, class T15, class T16, class T17, class T18, class T19, class T20, class T21, class T22, class T23, class T24, class T25, class T26, class T27, class T28, class T29, class T30, class T31, class...Ts>
               using f = ml::f<DelayedEval<Pivot<_Pivot::GetNext::f<N::value - 32>::value>,
                  sizeof...(Ts)>,
                  Pipe, Int<N::value - 32>, Ts..., T0, T1, T2, T3, T4, T5, T6, T7,
                  T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21,
                  T22, T23, T24, T25, T26, T27, T28, T29, T30, T31
               >;
            };

            struct Front {
               template <class Pipe, class T, class...Ts>
               using f = ml::f<Pipe, T>;
            };

            template<class...>
            struct Unwrap;

            template<class Pipe, template<class...> class List, class...Es>
            struct Unwrap<Pipe, List<Es...>> {
               using type = typename Pipe::template f<Es...>;
            };
         }

         /// Wraps the pack in a list                                         
         using ToList = F<Types>;

         /// Maps each element using `F`                                      
         template<class F, class Pipe = ToList>
         struct Map {
            template<class...Ts>
            using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>,
               typename Implementations::IfElse<(sizeof...(Ts) < 100000)>::
               template f<F, void>::template f<Ts>...>;
         };

         /// Pivots a parameter pack, around `N`                              
         /// `(A, ..., M, N, ..., Z) -> (N, ..., Z, A, ..., M)`               
         template <int N, class Pipe = ToList>
         struct Pivot {
            template <class...Ts>
            using f = ml::f<DelayedEval<Implementations::Pivot<
               Implementations::_Pivot::GetNext::f<N>::value>,
               sizeof...(Ts)>,
               Pipe, Int<N>, Ts...>;
         };

         template<class Pipe = Identity>
         struct Front {
            template <typename... Ts>
            using f = ml::f<DelayedEval<Implementations::Front, sizeof...(Ts)>, Pipe, Ts...>;
         };

         /// Retrieves the N - th element in a pack                           
         template<int N, class Pipe = Identity>
         struct Get {
            template<class...Ts>
            using f = ml::f<DelayedEval<Pivot<N, Front<Pipe>>, sizeof...(Ts)>, Ts...>;
         };

         template<class Pipe>
         struct Unwrap {
            template <class Ls>
            using f = typename Implementations::Unwrap<Pipe, Ls>::type;
         };
         
         /// Appends elements to a list - like structure                      
         template<class T, class Pipe = ToList>
         struct Append {
            template<class...Ts>
            using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>, Ts..., T>;
         };

         /// Rotates a pack, so that the subpack `[First, Last)`, is pivoted  
         /// around Middle                                                    
         /// (Start, ..., First, ..., M',  Middle, ..., L', Last, ... End)    
         /// ->                                                               
         /// (Start, ..., Middle, ..., L', First, ... M', Last, ...End)       
         template<int First, int Middle, int Last, class Pipe = ToList>
         struct Rotate {
            template<class...Ts>
            using f = ml::f<
               ml::DelayedEval<
               ml::Drop<
               First,
               ml::Head<
               Last - First,
               ml::Pivot<
               Middle - First,
               ml::f<
               ml::DelayedEval<
               ml::Head<First,
               ml::Curry<ml::f<
               ml::DelayedEval<
               ml::Drop<Last, ml::CurryR<Pipe>>,
               sizeof...(Ts)>,
               Ts...>>>,
               sizeof...(Ts)>,
               Ts...>>>>,
               sizeof...(Ts)>,
               Ts...>;
         };

         template<int First, int Last, class Pipe>
         struct Rotate<First, First, Last, Pipe> {
            template<class...Ts>
            using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>, Ts...>;
         };

         namespace Implementations {
            namespace Detail {
               /// This is used to short circuit when found.                  
               struct FindIfPipeOn {
                  template<class Prev, class I, class Getter, class Pipe, class...Ts>
                  using f = ml::f<Pipe, ml::f<Getter, Prev, Int<I::value - 1>>>;
               };
            }
            
            template<bool Continue>
            struct FindIf {
               template<class Prev, class I, class Getter, class Pipe, class Predicate, class T, class...Ts>
               using f = ml::f<DelayedEval<
                  ml::f<Implementations::IfElse<ml::f<Predicate, T>::value>,
                     Detail::FindIfPipeOn,
                     FindIf<(sizeof...(Ts) > 0)>
                  >,
                  sizeof...(Ts)>,
                  T, Int<I::value + 1>, Getter, Pipe, Predicate, Ts...
               >;
            };

            template<>
            struct FindIf<false> {
               template<class Prev, class I, class Getter, class Pipe, class Predicate>
               using f = ml::f<Pipe, ml::f<Getter, None, I>>;
            };

            template<class...>
            struct Zip;

            template<class With, class Pipe, template<class...> class Result, class...Rs>
            struct Zip<With, Pipe, Result<Rs...>> {
               using f = typename Pipe::template f<Rs...>;
            };

            template<class With, class Pipe, template<class...> class Result, class...Rs, template<class...> class Next, class... Ns, class...Rest>
            struct Zip<With, Pipe, Result<Rs...>, Next<Ns...>, Rest...> {
               using f = typename Zip<
                  With, Pipe,
                  Result<typename Unwrap<Append<Ns, With>>::template f<Rs>...>,
                  Rest...>::f;
            };

            struct ZipStart {
               template<class Pipe, template<class...> class With, class T, class...Ts>
               using f = typename Zip<F<With>, Pipe,
                  typename Unwrap<Map<F<With>>>::template f<T>,
                  Ts...>::f;
            };

            struct ZipForward {
               template<class Pipe, template<class...> class With, class...Ts>
               using f = ml::f<DelayedEval<Pipe, sizeof...(Ts)>, Ts...>;
            };

            template<bool Continue>
            struct Sort {
               template<class I, class Pipe, class Compare, class...Ts>
               using f = ml::f<
                  ml::Rotate<
                  ml::Invoke<
                  ml::FindIdIf<
                  ml::PartialR<
                  Compare,
                  ml::f<ml::DelayedEval<
                  ml::Get<I::value>,
                  sizeof...(Ts)>,
                  Ts...>>>,
                  Ts...
                  >::value +
                  3,
                  I::value + 3, I::value + 4,
                  Implementations::Sort<(I::value + 1 < sizeof...(Ts))>>,
                  Int<I::value + 1>, Pipe, Compare, Ts...>;
            };

            template<>
            struct Sort<false> {
               template<class I, class Pipe, class Compare, class...Ts>
               using f = ml::f<ml::DelayedEval<Pipe, sizeof...(Ts)>, Ts...>;
            };
         }
         
         /// Returns index of the element matching the predicate              
         template<class Predicate, class Pipe = ml::Identity>
         struct FindIdIf {
            template<class...Ts>
            using f = ml::f<Implementations::FindIf<(sizeof...(Ts) > 0)>,
               None, Int<0>, Get<1>, Pipe, Predicate, Ts...
            >;
         };

         /// Partial evaluation of a metafunction                             
         template<class F, class...Ts>
         struct Partial {
            template <class...Us>
            using f = ml::f<DelayedEval<F, (sizeof...(Us) + sizeof...(Ts))>, Ts..., Us...>;
         };

         namespace TypeTraitsImplementations
         {
            template<class T>
            struct IsSameHelper {
               static Bool<true>  test(const volatile IsSameHelper<T>*);
               static Bool<false> test(const volatile void*);
               template<class U>
               using f = decltype(IsSameHelper<T>::test(std::declval<IsSameHelper<U>*>()));
            };
         }

         template<class Pipe = Identity>
         struct IsSame {
            template<class T1, class T2>
            using f = typename Pipe::template f<typename TypeTraitsImplementations::IsSameHelper<T1>::template f<T2>>;
         };

         /// Zips a pack of types in a Zipper                                 
         template<template<class...> class With, class Pipe = ToList>
         struct ZipWith {
            template <class...Ts>
            using f =
               typename Implementations::IfElse<(sizeof...(Ts) > 0)>::template f<
               Implementations::ZipStart, Implementations::ZipForward>::
               template f<ml::Map<ml::Unwrap<ml::F<With>>, Pipe>, Types, Ts...>;
         };

         /// Sorts a parameter pack, given a comparator                       
         template<class Compare, class Pipe = ToList>
         struct Sort {
            template <class...Ts>
            using f = ml::f<Implementations::Sort<(sizeof...(Ts) > 1)>, Int<1>,
               Pipe, ml::Compose<ml::Not<>, Compare>, Ts...>;
         };
      }

      template<class Permutation, class Tuple>
      struct TupleBase;
   
      template<int...Is, class...Ts>
      struct TupleBase<Types<ml::Int<Is>...>, std::tuple<Ts...>> {
      private:
         std::tuple<Ts...> _tuple;
      
         template<class...Us>
         TupleBase(ml::_, std::tuple<Us...>&& fwd)
             : _tuple {FWD(std::get<Is>(fwd))...} {}

      public:
         template<class...Us>
         TupleBase(Us&&...us)
             : TupleBase {ml::_{}, std::forward_as_tuple(FWD(us)...)} {}
      
         template<class I> // Compute the inverse index
         using f = ml::f<ml::FindIdIf<ml::Partial<ml::IsSame<>, I>>, ml::Int<Is>...>;
      
         template<int I, class...Us>
         friend decltype(auto) get(TupleBase<Us...> &tup) {
            return std::get<ml::f<TupleBase<Us...>, ml::Int<I>>::value>(tup._tuple);
         }
      
         template<int I, class... Us>
         friend decltype(auto) get(const TupleBase<Us...> &tup) {
            return std::get<ml::f<TupleBase<Us...>, ml::Int<I>>::value>(tup._tuple);
         }
      };

      template<class...Ts>
      using MakeBase = ml::f<ml::ZipWith<
            Types,
            ml::Sort<
               ml::Map<ml::Unwrap<ml::Get<1, ml::AlignOf<>>>, ml::Greater<>>,
               ml::Product<
                  ml::Map<ml::Unwrap<ml::Get<0>>>,
                  ml::Map<ml::Unwrap<ml::Get<1>>, ml::F<std::tuple>>,
                  ml::F<TupleBase>
               >
            >
         >,
         ml::Range<>::f<0, sizeof...(Ts)>,
         Types<Ts...>
      >;
   }


   ///                                                                        
   /// Optimized tuple                                                        
   ///                                                                        
   ///   std::tuple doesn't pack sequences well, and produces a heck of a lot 
   /// of padding overhead. Thanks to Žiga Sajovic and category theory, we    
   /// can remedy the situation. This implementation is based on his here:    
   /// github.com/ZigaSajovic/optimizing-the-memory-layout-of-std-tuple       
   /// And check out his amazing C++ metalanguage library here:               
   /// https://github.com/ZigaSajovic/CppML                                   
   ///                                                                        
   template<class...Ts>
   struct Tuple : Inner::MakeBase<Ts...> {
      using Inner::MakeBase<Ts...>::MakeBase;
   };

   template<
      int...Is,
      template<class...> class T, class...Ts,
      template<class...> class U, class...Us,
      class F
   >
   decltype(auto) envoker(Types<ml::Int<Is>...>, const T<Ts...>& t, const U<Us...>& u, F&& f) {
      using std::get;
      return (... && f(get<Is>(t), get<Is>(u)));
   }

   template <class...Ts, class...Us>
   auto operator == (const std::tuple<Ts...> &lhs, const Tuple<Us...> &rhs) -> bool {
      using List = ml::Range<>::f<0, sizeof...(Ts)>;
      return envoker(List{}, lhs, rhs, [](auto &&x, auto &&y) { return x == y; });
   };

   template <class...Ts, class...Us>
   auto operator == (const Tuple<Us...> &lhs, const std::tuple<Ts...> &rhs) -> bool {
      return rhs == lhs;
   };
}
