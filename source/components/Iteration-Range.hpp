///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <ranges>


namespace Langulus::Anyness
{
   ///                                                                        
   ///   A weightless 'end' iterator helper type                              
   ///                                                                        
   /// Used to return from container's end() methods. It only compares equal  
   /// to other iterators if they have reached their end marker.              
   /*struct IteratorEnd final {
      using CTTI_Iterator  = Yes<>;
      using CTTI_ReflectAs = void;
   };*/

   
   ///                                                                        
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : IterateInReverse(container)), where        
   /// 'container' can be any range, including std one.                       
   template<::std::ranges::range C>
   struct IterateInReverse {
      using CTTI_ReflectAs = void;

      C& range;

      explicit constexpr IterateInReverse(C& a) noexcept
         : range {a} {}

      decltype(auto) begin() noexcept { return range.rbegin(); }
      decltype(auto) end()   noexcept { return range.rend();   }
   };

   template<::std::ranges::range C>
   IterateInReverse(C&) -> IterateInReverse<C>;


   ///                                                                        
   ///   Keep iterator when using ranged-for                                  
   ///                                                                        
   /// When doing for(auto i : container), the statement always dereferences  
   /// the iterator and 'i' always ends up with the contained type.           
   /// Counteract this, and make 'i' be the iterator type instead.            
   /// Use like this: for(auto i : IterateNoDeref(container)), where          
   /// 'container' can be any range, including std one                        
   template<bool REVERSE, ::std::ranges::range C>
   struct IterateNoDeref {
      using CTTI_ReflectAs = void;

   protected:
      using H = decltype(Fake<C>().begin());

      C& range;

   public:
      explicit constexpr IterateNoDeref(C& a) noexcept
         : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         mutable H mIt;
         C& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H const& it, C& range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (CT::Iterator auto const& rhs) const noexcept {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            return mIt == rhs.mIt;
         }

         /*bool operator == (const Iterator& rhs) const noexcept {
            return mIt == rhs.mIt;
         }

         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }*/
         
         explicit constexpr operator bool() const noexcept {
            if constexpr (REVERSE) return mIt != mRange.rend();
            else                   return mIt != mRange.end();
         }

         auto operator *  () const noexcept -> H& { return mIt; }
         auto operator -> () const noexcept -> H& { return mIt; }

         auto operator ++ () noexcept -> Iterator& {
            if constexpr (REVERSE) --mIt;
            else                   ++mIt;
            return *this;
         }

         auto operator ++ (int) noexcept -> Iterator {
            if constexpr (REVERSE) return mIt--;
            else                   return mIt++;
         }

         auto operator -- () noexcept -> Iterator& {
            if constexpr (REVERSE) ++mIt;
            else                   --mIt;
            return *this;
         }

         auto operator -- (int) noexcept -> Iterator {
            if constexpr (REVERSE) return mIt++;
            else                   return mIt--;
         }
      };

      auto begin() -> Iterator  {
         if constexpr (REVERSE) return {range.rbegin(), range};
         else                   return {range.begin(),  range};
      }

      auto end() -> Iterator {
         if constexpr (REVERSE) return {range.rend(), range};
         else                   return {range.end(),  range};
      }
   };

   template<::std::ranges::range C>
   IterateNoDeref(C&) -> IterateNoDeref<false, C>;


   ///                                                                        
   ///   Default iteration                                                    
   ///                                                                        
   /// Used by default when doing for(auto i : container)                     
   /// When container is type-erased, or mutable and sparse, 'i' will be a    
   /// handle. Otherwise, 'i' will be a direct reference to the element       
   template<bool REVERSE, CT::ContainsMany C>
   struct IterateDefault {
      static_assert(CT::NoIntent<C>,     "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      using CTTI_ReflectAs = void;

   protected:
      using Pick    = typename C::Pick;
      using PickMut = typename C::PickMut;

      // The handle is either a pointer/THandle for statically-typed    
      // containers, or Handle/HandleMut for type-erased ones           
      using H = Tmut<C,
         Tif<CT::Handle<Pick>,    PickMut, Deref<PickMut>*>,
         Tif<CT::Handle<PickMut>, Pick,    Deref<Pick>*>
      >;

      C& range;

   public:
      explicit constexpr IterateDefault(C& a) noexcept
         : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         mutable H mIt;
         C& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H const& it, C& range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (CT::Iterator auto const& rhs) const noexcept {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            return mIt == rhs.mIt;
         }

         /*constexpr bool operator == (const Iterator& rhs) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() == rhs.mIt.GetRaw();
            else
               return mIt == rhs.mIt;
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() == mRange.GetRawEnd();
            else
               return mIt == mRange.GetRawEnd();
         }*/
         
         explicit constexpr operator bool() const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() != mRange.GetRawEnd();
            else
               return mIt != mRange.GetRawEnd();
         }

         auto operator *  () const noexcept -> H& { return  mIt; }
         auto operator -> () const noexcept -> H* { return &mIt; }

         auto operator ++ () noexcept -> Iterator& {
            if constexpr (REVERSE) --mIt;
            else                   ++mIt;
            return *this;
         }

         auto operator ++ (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt--, mRange};
            else                   return {mIt++, mRange};
         }

         auto operator -- () noexcept -> Iterator& {
            if constexpr (REVERSE) ++mIt;
            else                   --mIt;
            return *this;
         }

         auto operator -- (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt++, mRange};
            else                   return {mIt--, mRange};
         }
      };

      constexpr auto begin() const noexcept -> Iterator {
         if constexpr (REVERSE)
            return {range.template AsAt<H>(range.GetCount() - 1), range};
         else
            return {range.template As<H>(), range};
      }

      constexpr auto end() const noexcept -> Iterator {
         if constexpr (REVERSE)
            return --Iterator{range.template As<H>(), range};
         else
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), range};
      }
   };

   template<CT::ContainsMany C>
   IterateDefault(C&) -> IterateDefault<false, C>;


   ///                                                                        
   ///   Iterate using handles                                                
   ///                                                                        
   /// When doing for(auto i : container), the statement always uses the most 
   /// optimal iteration approach, but often you want to be able to modify    
   /// values in-place while iterating.                                       
   /// Use like this: for(auto i : IterateHandles(container)), where          
   /// 'container' can be any CT::Container.                                  
   template<bool REVERSE, CT::Container C>
   struct IterateHandles {
      using CTTI_ReflectAs = void;
      static_assert(CT::NoIntent<C>,     "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      static_assert(CT::ContainsMany<C>, "C is not iteratable because it contains exactly one element");
      
   protected:
      using Count = typename Deref<C>::CountType;
      using H = DecideHandle<C>;

      C& range;

   public:
      explicit constexpr IterateHandles(C& a) noexcept
         : range{a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         mutable H mIt;
         C& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H const& it, C& range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (CT::Iterator auto const& rhs) const noexcept {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         /*constexpr bool operator == (const Iterator& rhs) const noexcept {
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEnd();
         }*/

         explicit constexpr operator bool() const noexcept {
            return mIt.GetRaw() != mRange.GetRawEnd();
         }
         
         auto operator *  () const noexcept -> H& { return  mIt; }
         auto operator -> () const noexcept -> H* { return &mIt; }

         auto operator + (Count c) const noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt - c, mRange};
            else                   return {mIt + c, mRange};
         }

         auto operator - (Count c) const noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt + c, mRange};
            else                   return {mIt - c, mRange};
         }
         
         auto operator ++ () noexcept -> Iterator& {
            if constexpr (REVERSE) --mIt;
            else                   ++mIt;
            return *this;
         }

         auto operator ++ (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt--, mRange};
            else                   return {mIt++, mRange};
         }

         auto operator -- () noexcept -> Iterator& {
            if constexpr (REVERSE) ++mIt;
            else                   --mIt;
            return *this;
         }

         auto operator -- (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt++, mRange};
            else                   return {mIt--, mRange};
         }

         /// Get the integer element difference between two iterators         
         auto operator - (const Iterator& rhs) const noexcept -> ptrdiff_t {
            if constexpr (CT::TypeErased<C>) {
               const auto range = mIt.template GetRawAs<uint8_t>() - rhs.mIt.template GetRawAs<uint8_t>();
               return static_cast<ptrdiff_t>(range / mRange.GetStride());
            }
            else {
               const auto range = mIt.GetRaw() - rhs.mIt.GetRaw();
               return static_cast<ptrdiff_t>(range);
            }
         }
      };

      constexpr Iterator begin() const noexcept {
         if constexpr (REVERSE)
            return {range.template AsAt<H>(range.GetCount() - 1), range};
         else
            return {range.template As<H>(), range};
      }

      constexpr Iterator end() const noexcept {
         if constexpr (REVERSE)
            return --Iterator{range.template As<H>(), range};
         else
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), range};
      }
   };

   template<CT::Container C>
   IterateHandles(C&) -> IterateHandles<false, C>;


   ///                                                                        
   ///   Iterate multiple containers with the same ranged-for                 
   ///                                                                        
   /// Use like this: for(auto i : IterateTogether(pack1, pack2)), where      
   /// 'packN' can be any range, including std one. You can retrieve the      
   /// current element by using i[N], or i.one() i.two() for the first two.   
   template<bool REVERSE, ::std::ranges::range...C>
   struct IterateTogether {
      using CTTI_ReflectAs = void;
      static constexpr size_t Count = sizeof...(C);
      static_assert(Count > 1,
         "IterateTogether needs at least two containers");

   protected:
      using Hs = ::std::tuple<decltype(Fake<C>().begin())...>;
      using Cs = ::std::tuple<C&...>;

      Cs range;

   public:
      explicit constexpr IterateTogether(C&...a) noexcept
         : range {a...} {}

      /// A single combined iterator                                          
      struct Iterator {
         using CTTI_Iterator = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         mutable Hs mIt;
         Cs mRanges;

      public:
         decltype(auto) one() noexcept { return ::std::get<0>(mIt); }
         decltype(auto) two() noexcept { return ::std::get<1>(mIt); }

         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(Hs const& it, Cs& ranges) noexcept
            : mIt    {it}
            , mRanges{ranges} {}
         constexpr Iterator(Hs&& it, Cs& ranges) noexcept
            : mIt    {FWD(it)}
            , mRanges{ranges} {}

         /*bool operator == (const Iterator& rhs) const noexcept {
            return mIt == rhs.mIt;
         }

         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }*/
         
         explicit constexpr operator bool() const noexcept {
            return LglsSequence(Count, {
               return ((::std::get<I>(mIt) != ::std::get<I>(mRanges).end()) and ...);
            });
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
   IterateTogether(C&...) -> IterateTogether<false, C...>;
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements ranged iteration interface for containers                   
   ///   @tparam ID - heap/stack we're iterating                              
   template<unsigned ID>
   struct IterationRange {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Iterator = typename IterateDefault<false, Deref<C>>::Iterator;
      template<CT::Container C>
      using IteratorRev = typename IterateDefault<true, Deref<C>>::Iterator;

   public:
      /// Return an iterator to the first element                             
      template<CT::Container C>
      constexpr auto begin(this C&& self) noexcept {
         return IterateDefault(self).begin();
      }

      template<CT::Container C>
      constexpr auto end(this C const& self) noexcept {
         return IterateDefault(self).end();
      }

      /// Return the last item                                                
      /*template<CT::Container C>
      constexpr auto last(this C&& self) noexcept -> Iterator<C> {
         const auto offset = self.IsEmpty() ? 0 : self.GetCount() - 1;

         if constexpr (CT::TypeErased<C> or (CT::Mutable<C> and Deref<C>::Sparse))
            return {self.GetHandle() + offset, self};
         else
            return {self.GetRaw() + offset, self};
      }

      /// Return a reverse iterator to the last element                       
      template<CT::Container C>
      constexpr auto rbegin(this C&& self) noexcept -> IteratorRev<C> {
         return self.last();
      }*/


      //constexpr auto rend() const noexcept -> IteratorEnd { return {}; }
   };
}
