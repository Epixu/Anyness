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
   /// Used to return from container's end() methods. It only compares        
   /// equal to other iterators if they have reached their end marker.        
   struct IteratorEnd final {
      using CTTI_Iterator  = Yes<>;
      using CTTI_ReflectAs = void;
   };

   
   ///                                                                        
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : IterateInReverse(container)), where        
   /// 'container' can be any range, including std one.                       
   template<::std::ranges::range C>
   struct IterateInReverse {
      using CTTI_ReflectAs = void;

      C& range;

      explicit constexpr IterateInReverse(C& a) noexcept : range {a} {}

      decltype(auto) begin() noexcept { return range.rbegin(); }
      decltype(auto) end()   noexcept { return range.rend();   }
   };

   template<::std::ranges::range C>
   IterateInReverse(C&) -> IterateInReverse<C>;


   ///                                                                        
   ///   Keep iterator when using ranged-for                                  
   ///                                                                        
   /// When doing for(auto i : container), the statement always               
   /// dereferences the iterator and 'i' always ends up with the contained    
   /// type. Counteract this, and make 'i' be the iterator type instead.      
   /// Use like this: for(auto i : IterateNoDeref(container)), where          
   /// 'container' can be any range, including std one                        
   template<::std::ranges::range C>
   struct IterateNoDeref {
      using CTTI_ReflectAs = void;

      C& range;

      explicit constexpr IterateNoDeref(C& a) noexcept : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         using T = decltype(Fake<C>().begin());
         T mIt;

      public:
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

         T& operator *  () const noexcept { return mIt; }
         T& operator -> () const noexcept { return mIt; }

         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return mIt++; }
         Iterator& operator -- ()    noexcept { --mIt; return *this; }
         Iterator  operator -- (int) noexcept { return mIt--; }
      };

      Iterator       begin() { return Iterator {range.begin()}; }
      decltype(auto) end  () { return range.end(); }
   };

   template<::std::ranges::range C>
   IterateNoDeref(C&) -> IterateNoDeref<C>;


   ///                                                                        
   ///   Default iteration                                                    
   ///                                                                        
   /// Used by default when doing for(auto i : container)                     
   /// When container is type-erased, or mutable and sparse, 'i' will be a    
   /// handle. Otherwise, 'i' will be a direct reference to the element       
   template<CT::Container C>
   struct IterateDefault {
      static_assert(CT::NoIntent<C>, "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      using CTTI_ReflectAs = void;
      using Pick    = typename C::Pick;
      using PickMut = typename C::PickMut;

      C& range;

      explicit constexpr IterateDefault(C& a) noexcept : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
         using H = Tmut<C,
            Tif<CT::NotReference<Pick>,    PickMut, Deref<PickMut>*>,
            Tif<CT::NotReference<PickMut>, Pick,    Deref<Pick>*>
         >;

         mutable H mIt;
         C const& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H&& it, const C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (const Iterator& rhs) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() == rhs.mIt.GetRaw();
            else
               return mIt == rhs.mIt;
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() == mRange.GetRawEnd();
            else
               return mIt == mRange;
         }

         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return {mIt++, mRange}; }
         Iterator& operator -- ()    noexcept { --mIt; return *this; }
         Iterator  operator -- (int) noexcept { return {mIt--, mRange}; }
      };

      constexpr Iterator begin() const noexcept {
         return Iterator {range.begin()};
      }
      constexpr decltype(auto) end() const noexcept { return range.end(); }
   };

   template<CT::Container C>
   IterateDefault(C&) -> IterateDefault<C>;
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
      using Iterator = typename IterateDefault<Deref<C>>::Iterator;
      template<CT::Container C>
      using IteratorRev = typename IterateInReverse<Deref<C>>::Iterator;

   public:
      /// Return an iterator to the first element                             
      template<CT::Container C>
      constexpr auto begin(this C&& self) noexcept -> Iterator<C> {
         if constexpr (CT::TypeErased<C> or (CT::Mutable<C> and Deref<C>::Sparse))
            return {self.GetHandle(), self};
         else
            return {self.GetRaw(), self};
      }

      /// Return the last item                                                
      template<CT::Container C>
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
      }

      constexpr auto end()  const noexcept -> IteratorEnd { return {}; }
      constexpr auto rend() const noexcept -> IteratorEnd { return {}; }
   };
}
