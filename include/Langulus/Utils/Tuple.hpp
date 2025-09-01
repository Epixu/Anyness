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
         //template <bool N> using Bool = Value<bool, N>;
         //template <char C> using Char = Value<char, C>;

         /// Used in tag-dispatch patterns                                    
         struct _ {};

         /// Invokes a meta function                                          
         template<class Pipe, class...Ts>
         using f = typename Pipe::template f<Ts...>;

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
               using f = ml::f<ml::DelayedEval<
                  ml::f<ml::Implementations::IfElse<ml::f<Predicate, T>::value>,
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
               using f = ml::f<Pipe, ml::f<Getter, ml::None, I>>;
            };
         }
         
         /// Returns index of the element matching the predicate              
         template<class Predicate, class Pipe = ml::Identity>
         struct FindIdIf {
            template<class...Ts>
            using f = ml::f<Implementations::FindIf<(sizeof...(Ts) > 0)>,
               ml::None, Int<0>, ml::Get<1>, Pipe, Predicate, Ts...
            >;
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
