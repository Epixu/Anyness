///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Iteration-Range.hpp"


namespace Langulus::Anyness
{
   ///                                                                        
   ///   Iterate multiple containers with the same ranged-for                 
   ///                                                                        
   /// Use like this: for(auto i : IterateTogether(pack1, pack2)), where      
   /// 'packN' can be any range, including std one. You can retrieve the      
   /// current element by using i[N], or i.one() i.two() for the first two.   
   ///                                                                        
   template<::std::ranges::range...C>
   struct IterateTogether {
      using CTTI_ReflectAs = void;
      static_assert(sizeof...(C) > 1,
         "IterateTogether needs at least two containers");

      ::std::tuple<C&...> range;

      explicit constexpr IterateTogether(C&...a) noexcept : range {a...} {}

      /// A single combined iterator                                          
      struct Iterator {
         using CTTI_Iterator = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         using T = ::std::tuple<decltype(Fake<C>().begin())...>;
         T mIt;

      public:
         decltype(auto) one() noexcept { return ::std::get<0>(mIt); }
         decltype(auto) two() noexcept { return ::std::get<1>(mIt); }

         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(const T& it) noexcept : mIt {it} {}

         bool operator == (const Iterator& rhs) const noexcept {
            return mIt == rhs.mIt;
         }
         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }

         Iterator& operator *  () const noexcept { return *this; }
         Iterator& operator -> () const noexcept { return *this; }

         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return mIt++; }
         Iterator& operator -- ()    noexcept { --mIt; return *this; }
         Iterator  operator -- (int) noexcept { return mIt--; }
      };

      Iterator begin() {
         return ::std::apply([](auto&...i) {
            return Iterator {{i.begin()...}};
            }, range);
      }

      Iterator end() {
         return ::std::apply([](auto&...i) {
            return Iterator {{i.end()...}};
            }, range);
      }
   };

   template<::std::ranges::range...C>
   IterateTogether(C&...) -> IterateTogether<C...>;
}
