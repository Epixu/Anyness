/// Optimal layout tuple                                                      
/// Written in 2012 by Martinho Fernandes <martinho.fernandes@gmail.com>      
/// Modified and modernized for C++23 in 2025 by Dimo Markov                  
/// <team@langulus.com>. Changes made:                                        
///  - making tuple fully constexpr                                           
///  - using 'requires' instead of 'std::enable_if' patterns                  
///  - using concepts for some require checks                                 
///                                                                           
/// SPDX-License-Identifier: CC0-1.0                                          
#pragma once
#include <tuple>
#include <type_traits>
#include <utility>
#include <functional>
#include <memory>
#include <cstddef>


namespace Langulus::Inner
{
   /// Compile-time max function                                              
   template<size_t Acc, size_t Head = 0, size_t...Tail>
   consteval size_t max() {
      if constexpr (sizeof...(Tail) == 0)
         return Acc > Head ? Acc : Head;
      else if constexpr (Acc > Head)
         return max<Acc, Tail...>();
      else
         return max<Head, Tail...>();
   }

   /// Indices                                                                
   template<size_t...I>
   struct indices {};
   
   template<size_t I>
   using index = std::integral_constant<size_t, I>;
}

namespace std
{
   template<size_t I, size_t...Is>
   struct tuple_element<I, ::Langulus::Inner::indices<Is...>>
      : tuple_element<I, tuple< ::Langulus::Inner::index<Is>...>> {};
}

namespace Langulus::Inner
{
   template<size_t...N>
   struct indices_builder {
      using type = indices<N...>;
      using next = indices_builder<N..., sizeof...(N)>;
   };
   
   template<size_t N>
   struct indices_up_to {
      using builder = typename indices_up_to<N - 1>::builder::next;
      using type = typename builder::type;
   };
   
   template<>
   struct indices_up_to<0> {
      using builder = indices_builder<>;
      using type = ::std::tuple<>;
   };
   
   template<class Tuple>
   using IndicesFor = typename indices_up_to<std::tuple_size_v<Tuple>>::type;

   template<class T>
   struct unwrap_reference : std::type_identity<T> {};
   
   template<class T>
   struct unwrap_reference<std::reference_wrapper<T>> : std::type_identity<T&>  {};

   template<class T>
   struct decay_reference : unwrap_reference<Decay<T>> {};
   
   template<class T>
   using DecayReference = typename decay_reference<T>::type;

   /// Wrapper to get alignment of references                                 
   template<class T>
   struct member { T _; };

   /// Packing types with indices                                             
   template<class T, size_t I>
   struct indexed {
      using type = T;
      static constexpr auto i = I;
   };

   /// Attaching index information                                            
   template<class Acc, class...T>
   struct with_indices_impl
      : std::type_identity<Acc> {};
   
   template<class...Acc, class Head, class...Tail>
   struct with_indices_impl<std::tuple<Acc...>, Head, Tail...>
      : with_indices_impl<std::tuple<Acc..., indexed<Head, sizeof...(Acc)>>, Tail...> {};

   template<class List>
   struct with_indices;
   
   template<class...T>
   struct with_indices<std::tuple<T...>>
      : with_indices_impl<std::tuple<>, T...> {};
   
   template<class List>
   using WithIndices = typename with_indices<List>::type;

   /// Building lists by appending at the appropriate end                     
   template<class Head, class Tail>
   struct cons;
   
   template<class Head, class...Tail>
   struct cons<Head, std::tuple<Tail...>>
      : std::type_identity<std::tuple<Tail..., Head>> {};

   template<class Head, class Tail>
   using Cons = typename cons<Head, Tail>::type;

   /// Compute alignments                                                     
   template<class T>
   struct alignof_indexed;
   
   template<class T, size_t I>
   struct alignof_indexed<indexed<T, I>>
      : std::alignment_of<member<T>> {};

   /// Find maximum alignment                                                 
   template<class List>
   struct max_alignment;
   
   template<class...T>
   struct max_alignment<std::tuple<T...>>
      : index<max<alignof_indexed<T>::value...>()> {};

   /// Cons all the types with a given alignment into an accumulator          
   template<size_t Align, class Acc, class List>
   struct cons_alignment : std::type_identity<Acc> {};

   template<size_t Align, class Acc, class Head, class...Tail>
   struct cons_alignment<Align, Acc, std::tuple<Head, Tail...>>
      : cons_alignment<
      Align,
      Tif<alignof_indexed<Head>::value == Align, Cons<Head, Acc>, Acc>,
      std::tuple<Tail...>> {};
   
   template<size_t Align, class Acc, class List>
   using ConsAlignment = typename cons_alignment<Align, Acc, List>::type;

   /// Sort by iterating down from the maximum alignment                      
   template<size_t Align, class Acc, class List>
   struct sort_impl
      : sort_impl<Align / 2, ConsAlignment<Align, Acc, List>, List> {};
   
   template<class Acc, class List>
   struct sort_impl<0, Acc, List> : std::type_identity<Acc> {};

   template<class List>
   struct sort : sort_impl<max_alignment<List>::value, std::tuple<>, List> {};
   
   template<class List>
   using Sort = typename sort<List>::type;

   /// Splitting the type and index information                               
   template<class List>
   struct split;
   
   template<class...T, size_t...I>
   struct split<std::tuple<indexed<T, I>...>> {
      using tuple = std::tuple<T...>;
      using map = indices<I...>;
   };

   /// Reversing the map                                                      
   template<class List>
   struct inherit_all;
   template<class...T>
   struct inherit_all<std::tuple<T...>> : T... {};

   template<class T, size_t I, size_t J>
   using indexed2 = indexed<indexed<T, I>, J>;

   template<size_t Target, size_t Result, class T>
   auto find_index_impl(indexed2<T, Target, Result> const&) -> index<Result>;

   template<size_t N, class List>
   using find_index = decltype(find_index_impl<N>(inherit_all<List>{}));

   template<class List, class Indices = IndicesFor<List>>
   struct map_to_storage {};
   
   template<class List, std::size_t... I>
   struct map_to_storage<List, indices<I...>>
      : std::type_identity<indices<find_index<I, List>::value...>> {};

   /// All the optimal layout info                                            
   template<class List>
   struct optimal_order {
      using sorted       = Sort<WithIndices<List>>;
      using tuple        = typename split<sorted>::tuple;
      using to_interface = typename split<sorted>::map;
      using to_storage   = typename map_to_storage<WithIndices<sorted>>::type;
   };

   template<class...T>
   using OptimalStorage = typename optimal_order<std::tuple<T...>>::tuple;
   
   template<class...T>
   using MapToInterface = typename optimal_order<std::tuple<T...>>::to_interface;
   
   template<class...T>
   using MapToStorage   = typename optimal_order<std::tuple<T...>>::to_storage;

   template<class Tuple, size_t...I>
   using ShuffleTuple   = std::tuple<std::tuple_element_t<I, Decay<Tuple>>...>;

   template<size_t...I, class Tuple>
   constexpr auto forward_shuffled_tuple(indices<I...>, Tuple&& t)
   -> ShuffleTuple<Tuple, I...> {
      using std::get;
      return std::forward_as_tuple(get<I>(FWD(t))...);
   }
   
   template<size_t...I, class...T>
   constexpr auto forward_shuffled(indices<I...> map, T&&... t)
   -> ShuffleTuple<std::tuple<T&&...>, I...> {
      return forward_shuffled_tuple(map, std::forward_as_tuple(FWD(t)...));
   }
   
   template<class From, class To>
   struct convert_layout_map;

   template<class From, size_t...To>
   struct convert_layout_map<From, indices<To...>>
      : std::type_identity<indices<std::tuple_element_t<To, From>::value...>> {};

   template<class From, class To>
   using ConvertLayoutMap = typename convert_layout_map<From, To>::type;

   template<size_t I, class...T>
   using PackElement = std::tuple_element_t<I, std::tuple<T...>>;
}

namespace Langulus
{
   ///                                                                        
   /// A tuple with optimized layout                                          
   /// Please, add it to 'std', please!                                       
   ///                                                                        
   template<class...T>
   struct compact_tuple : private Inner::OptimalStorage<T...> {
      using CTTI_Tuple   = Yes<>;
      using storage_type = Inner::OptimalStorage<T...>;
      using to_interface = Inner::MapToInterface<T...>;
      using to_storage   = Inner::MapToStorage<T...>;
      
      template<class...U>
      using MapFor = Inner::ConvertLayoutMap<typename compact_tuple<U...>::to_interface, to_interface>;

      constexpr  compact_tuple() = default;
      constexpr ~compact_tuple() {}

      explicit constexpr compact_tuple(T const&...t)
         : storage_type {forward_shuffled(to_interface{}, t...)} {
         static_assert((std::is_copy_constructible_v<T> and ...),
            "All elements must be copy-constructible");
      }
      
      template<class...U>
      requires (std::is_convertible_v<U, T> and ...)
      explicit constexpr compact_tuple(U&&...u)
         : storage_type {forward_shuffled(to_interface{}, FWD(u)...)} {}

      constexpr compact_tuple(compact_tuple const&) = default;
      constexpr compact_tuple(compact_tuple&&) = default;

      template<class...U>
      requires (std::is_constructible_v<T, U const&> and ...)
      constexpr compact_tuple(compact_tuple<U...> const& t)
         : storage_type {forward_shuffled_tuple(MapFor<U...>{}, t)} {}
      
      template<class...U>
      requires (std::is_constructible_v<T, U&&> and ...)
      constexpr compact_tuple(compact_tuple<U...>&& t)
         : storage_type {forward_shuffled_tuple(MapFor<U...>{}, MOV(t))} {}

      template<class U1, class U2>
      requires (std::is_convertible_v<U1 const&, Inner::PackElement<0, T...>>
            and std::is_convertible_v<U2 const&, Inner::PackElement<1, T...>>)
      constexpr compact_tuple(std::pair<U1, U2> const& p)
         : compact_tuple {p.first, p.second} {
         static_assert(sizeof...(T) == 2, "Tuple size must be 2");
      }
      
      template<class U1, class U2>
      requires (std::is_convertible_v<U1 const&, Inner::PackElement<0, T...>>
            and std::is_convertible_v<U2 const&, Inner::PackElement<1, T...>>)
      constexpr compact_tuple(std::pair<U1, U2>&& p)
         : compact_tuple {MOV(p.first), MOV(p.second)} {
         static_assert(sizeof...(T) == 2, "Tuple size must be 2");
      }

      template<class...U>
      requires (std::is_constructible_v<T, U const&> and ...)
      constexpr compact_tuple(std::tuple<U...> const& t)
         : compact_tuple {forward_shuffled_tuple(to_interface{}, t)} {}
      
      template<class...U>
      requires (std::is_constructible_v<T, U&&> and ...)
      constexpr compact_tuple(std::tuple<U...>&& t)
         : compact_tuple {forward_shuffled_tuple(to_interface{}, MOV(t))} {}

      template<class Alloc>
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a)
         : storage_type {tag, a} {}

      template<class Alloc>
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, T const&... t)
         : storage_type {tag, a, forward_shuffled(to_interface{}, t...)} {
         static_assert((std::is_copy_constructible_v<T> and ...),
            "All elements must be copy constructible");
      }
      
      template<class Alloc, class...U>
      requires (std::is_convertible_v<U, T> and ...)
      explicit constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, U&&... u)
         : storage_type {tag, a, forward_shuffled(to_interface{}, FWD(u)...)} {}

      template<class Alloc>
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, compact_tuple const& t)
         : storage_type {tag, a, t} {}
      
      template<class Alloc>
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, compact_tuple&& t)
         : storage_type {tag, a, MOV(t)} {}

      template<class Alloc, class...U>
      requires (std::is_constructible_v<T, U const&> and ...)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, compact_tuple<U...> const& t)
         : storage_type {tag, a, forward_shuffled_tuple(MapFor<U...>{}, t)} {}
      
      template<class Alloc, class... U>
      requires (std::is_constructible_v<T, U&&> and ...)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, compact_tuple<U...>&& t)
         : storage_type {tag, a, forward_shuffled_tuple(MapFor<U...>{}, MOV(t))} {}

      template<class Alloc, class U1, class U2>
      requires (std::is_convertible_v<U1 const&, Inner::PackElement<0, T...>>
            and std::is_convertible_v<U2 const&, Inner::PackElement<1, T...>>)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, std::pair<U1, U2> const& p)
         : compact_tuple {tag, a, p.first, p.second} {
         static_assert(sizeof...(T) == 2, "tuple size must be 2");
      }
      template<class Alloc, class U1, class U2>
      requires (std::is_convertible_v<U1 const&, Inner::PackElement<0, T...>>
            and std::is_convertible_v<U2 const&, Inner::PackElement<1, T...>>)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, std::pair<U1, U2>&& p)
         : compact_tuple {tag, a, MOV(p.first), MOV(p.second)} {
         static_assert(sizeof...(T) == 2, "tuple size must be 2");
      }

      template<class Alloc, class...U>
      requires (std::is_constructible_v<T, U const&> and ...)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, std::tuple<U...> const& t)
         : compact_tuple {tag, a, forward_shuffled_tuple(to_interface{}, t)} {}
      
      template<class Alloc, class...U>
      requires (std::is_constructible_v<T, U&&> and ...)
      constexpr compact_tuple(std::allocator_arg_t tag, Alloc const& a, std::tuple<U...>&& t)
         : compact_tuple {tag, a, forward_shuffled_tuple(to_interface{}, MOV(t))} {}

      constexpr compact_tuple& operator=(compact_tuple const&) = default;
      constexpr compact_tuple& operator=(compact_tuple&&) = default;

      template<class...U>
      constexpr compact_tuple& operator = (compact_tuple<U...> const& t) {
         static_assert((std::is_assignable_v<T&, U const&> and ...),
            "All elements must be assignable to the corresponding element");
         storage_type::operator=(forward_shuffled_tuple(MapFor<U...>{}, t));
         return *this;
      }
      
      template<class...U>
      constexpr compact_tuple& operator = (compact_tuple<U...>&& t) {
         static_assert((std::is_assignable_v<T&, U&&> and ...),
            "all elements must be move-assignable to the corresponding element");
         storage_type::operator=(forward_shuffled_tuple(MapFor<U...>{}, MOV(t)));
         return *this;
      }

      template<class U1, class U2>
      constexpr compact_tuple& operator = (std::pair<U1, U2> const& p) {
         static_assert(sizeof...(T) == 2, "tuple size must be 2");
         static_assert(std::is_assignable_v<Inner::PackElement<0, T...>&, U1 const&>,
            "first pair element must be assignable to first tuple element");
         static_assert(std::is_assignable_v<Inner::PackElement<1, T...>&, U2 const&>,
            "second pair element must be assignable to second tuple element");
         storage_type::operator=(forward_shuffled(to_interface {}, p.first, p.second));
         return *this;
      }
      
      template<class U1, class U2>
      constexpr compact_tuple& operator = (std::pair<U1, U2>&& p) {
         static_assert(sizeof...(T) == 2, "tuple size must be 2");
         static_assert(std::is_assignable_v<Inner::PackElement<0, T...>&, U1&&>,
            "first pair element must be move-assignable to first tuple element");
         static_assert(std::is_assignable_v<Inner::PackElement<1, T...>&, U2&&>,
            "second pair element must be move-assignable to second tuple element");
         storage_type::operator=(forward_shuffled(to_interface {}, MOV(p.first), MOV(p.second)));
         return *this;
      }

      template<class...U>
      constexpr compact_tuple& operator = (std::tuple<U...> const& t) {
         static_assert((std::is_assignable_v<T&, U const&> and ...),
            "all elements must be assignable to the corresponding element");
         storage_type::operator=(forward_shuffled_tuple(to_interface {}, t));
         return *this;
      }
      
      template<class... U>
      constexpr compact_tuple& operator = (std::tuple<U...>&& t) {
         static_assert((std::is_assignable_v<T&, U&&> and ...),
            "all elements must be move-assignable to the corresponding element");
         storage_type::operator=(forward_shuffled_tuple(to_interface {}, MOV(t)));
         return *this;
      }

      constexpr void swap(compact_tuple& t) noexcept(noexcept(storage_type::swap(t))) {
         storage_type::swap(t);
      }

      template<class...U>
      friend struct compact_tuple;
      template<size_t I, class...U>
      friend constexpr auto get(compact_tuple<U...>& t) -> std::tuple_element_t<I, std::tuple<U...>>&;
      template<size_t I, class...U>
      friend constexpr auto get(compact_tuple<U...>&& t) -> std::tuple_element_t<I, std::tuple<U...>>&&;
      template<size_t I, class...U>
      friend constexpr auto get(compact_tuple<U...> const& t) -> std::tuple_element_t<I, std::tuple<U...>> const&;
      template<class...L, class...R>
      friend constexpr bool operator == (compact_tuple<L...> const& l, compact_tuple<R...> const& r);
      template<class...L, class...R>
      friend constexpr bool operator <  (compact_tuple<L...> const& l, compact_tuple<R...> const& r);
   };

   template<size_t I, class...U>
   constexpr auto get(compact_tuple<U...>& t) -> std::tuple_element_t<I, std::tuple<U...>>& {
      return std::get<std::tuple_element_t<I, Inner::MapToStorage<U...>>::value>(t);
   }
   
   template<size_t I, class...U>
   constexpr auto get(compact_tuple<U...>&& t) -> std::tuple_element_t<I, std::tuple<U...>>&& {
      return std::get<std::tuple_element_t<I, Inner::MapToStorage<U...>>::value>(t);
   }
   
   template<size_t I, class...U>
   constexpr auto get(compact_tuple<U...> const& t) -> std::tuple_element_t<I, std::tuple<U...>> const& {
      return std::get<std::tuple_element_t<I, Inner::MapToStorage<U...>>::value>(t);
   }

   template<class...T>
   constexpr auto make_tuple(T&&...t) -> compact_tuple<Inner::DecayReference<T>...> {
      return {FWD(t)...};
   }
   
   template<class...T>
   constexpr auto forward_as_tuple(T&&... t) noexcept -> compact_tuple<T&&...> {
      return {FWD(t)...};
   }
   
   template<class...T>
   constexpr auto tie(T&... t) noexcept -> compact_tuple<T&...> {
      return {t...};
   }

   template<class...T>
   constexpr void swap(compact_tuple<T...>& x, compact_tuple<T...>& y) noexcept(noexcept(x.swap(y))) {
      x.swap(y);
   }

   template<class...T, class...U>
   constexpr bool operator == (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return static_cast<Inner::OptimalStorage<T...> const&>(t) == u;
   }
   
   template<class...T, class...U>
   constexpr bool operator <  (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return static_cast<Inner::OptimalStorage<T...> const&>(t) < u;
   }
   
   template<class...T, class...U>
   constexpr bool operator != (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return !(t == u);
   }
   
   template<class...T, class...U>
   constexpr bool operator >  (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return u < t;
   }
   
   template<class...T, class...U>
   constexpr bool operator <= (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return !(u < t);
   }
   
   template<class...T, class...U>
   constexpr bool operator >= (compact_tuple<T...> const& t, compact_tuple<U...> const& u) {
      return !(t < u);
   }
}

namespace std
{
   template<class...T>
   struct tuple_size<::Langulus::compact_tuple<T...>>
      : tuple_size<tuple<T...>> {};

   template<size_t I, class...T>
   struct tuple_element<I, ::Langulus::compact_tuple<T...>>
      : tuple_element<I, tuple<T...>> {};

   template<class...T, class Alloc>
   struct uses_allocator<::Langulus::compact_tuple<T...>, Alloc>
      : true_type {};
}
