///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/IntentOf.hpp>
#include <ranges>


namespace Langulus::Anyness
{
   ///                                                                        
   ///   Default iteration                                                    
   ///                                                                        
   /// Used by default when doing `for(auto i : container)`.                  
   /// When container is type-erased, or mutable and sparse, 'i' will be a    
   /// handle. Otherwise, 'i' will be a direct reference to the element.      
   template<bool REVERSE, class C>
   struct IterateDefault {
      using CTTI_ReflectAs = void;

      static_assert(CT::NoIntent<C>,
         "C can't have an intent");
      static_assert(CT::NotReference<C>,
         "C can't be a reference");
      static_assert(CT::ContainsMany<C>,
         "C is not iteratable because it contains exactly one element");
      static_assert(CT::Indexed<C>,
         "C is not indexed");

   protected:
      using Pick = DecidePick<C>;
      /*using Pick    = typename C::Pick;
      using PickMut = typename C::PickMut;
      using Count   = typename Deref<C>::CountType;*/

      // The handle is either a pointer/THandle for statically-typed    
      // containers, or Handle/HandleMut for type-erased ones           
      /*using H = Tmut<C,
         Tif<CT::Handle<PickMut>,   PickMut,          Deref<PickMut>*>,
         Tif<CT::Handle<Pick>,      Pick,    ConstAll<Deref<Pick>*>>
      >;*/
      using H = Tif<CT::Reference<Pick>, Deref<Pick>*, Pick>;
      static_assert(CT::NotReference<H>,
         "Iterator can't be a reference");
      static_assert(CT::Handle<H> or CT::Sparse<H>,
         "Must be either a pointer, or a handle");

      C& range;

   public:
      explicit constexpr IterateDefault(C& a) noexcept
         : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_ReflectAs    = void;
         using CTTI_Iterator     = Yes<>;
         using difference_type   = std::ptrdiff_t;
         using iterator_category = typename C::IteratorCategory;
         using value_type        = Deptr<H>;
         using reference         = Deptr<H>&;

         mutable H mIt;
         C* mRange;

         constexpr Iterator() noexcept = default;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(Deptr<H> const& it, C* range) noexcept requires CT::Sparse<H>
            : mIt    {&it}
            , mRange {range} {}
         constexpr Iterator(H const& it, C* range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C* range) noexcept
            : mIt    {LglsFwd(it)}
            , mRange {range} {}

         constexpr auto operator = (Iterator const& rhs) assumptious -> Iterator& {
            LglsAssumeUser(mRange == rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr auto operator = (Iterator&& rhs) assumptious -> Iterator& {
            LglsAssumeUser(mRange == rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr bool operator == (CT::Iterator auto const& rhs) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() == rhs.mIt.GetRaw();
            else
               return mIt == rhs.mIt;
         }

         constexpr auto operator <=> (CT::Iterator auto const& rhs) const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() <=> rhs.mIt.GetRaw();
            else
               return mIt <=> rhs.mIt;
         }
         
         explicit constexpr operator bool() const noexcept {
            if constexpr (CT::Handle<H>)
               return mIt.GetRaw() != mRange->GetRawEnd();
            else
               return mIt != mRange->GetRawEnd();
         }

         decltype(auto) operator * () noexcept {
            if constexpr (CT::Handle<H>) return (mIt);
            else                         return *mIt;
         }

         decltype(auto) operator * () const noexcept {
            if constexpr (CT::Handle<H>) return (mIt);
            else                         return *mIt;
         }

         decltype(auto) operator -> () noexcept {
            if constexpr (CT::Handle<H>) return &mIt;
            else                         return mIt;
         }

         decltype(auto) operator -> () const noexcept {
            if constexpr (CT::Handle<H>) return &mIt;
            else                         return mIt;
         }

         friend auto operator + (difference_type lhs, Iterator const& rhs) noexcept -> Iterator {
            static_assert(not REVERSE);
            return {rhs.mIt + lhs, rhs.mRange};
         }

         auto operator + (difference_type c) const noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt - c, mRange};
            else                   return {mIt + c, mRange};
         }

         auto operator - (difference_type c) const noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt + c, mRange};
            else                   return {mIt - c, mRange};
         }

         auto operator += (difference_type c) noexcept -> Iterator& {
            if constexpr (REVERSE) mIt -= c;
            else                   mIt += c;
            return *this;
         }

         auto operator -= (difference_type c) noexcept -> Iterator& {
            if constexpr (REVERSE) mIt += c;
            else                   mIt -= c;
            return *this;
         }

         decltype(auto) operator[] (const difference_type offset) const noexcept {
            if constexpr (REVERSE) {
               if constexpr (CT::Handle<H>) return   mIt - offset;
               else                         return *(mIt - offset);
            }
            else {
               if constexpr (CT::Handle<H>) return   mIt + offset;
               else                         return *(mIt + offset);
            }
         }

         /// Prefix increment                                                 
         auto operator ++ () noexcept -> Iterator& {
            if constexpr (REVERSE) --mIt;
            else                   ++mIt;
            return *this;
         }

         /// Suffix increment                                                 
         auto operator ++ (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt--, mRange};
            else                   return {mIt++, mRange};
         }

         /// Prefix decrement                                                 
         auto operator -- () noexcept -> Iterator& {
            if constexpr (REVERSE) ++mIt;
            else                   --mIt;
            return *this;
         }

         /// Suffix decrement                                                 
         auto operator -- (int) noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt++, mRange};
            else                   return {mIt--, mRange};
         }

         /// Get the integer element difference between two iterators         
         auto operator - (CT::Iterator auto const& rhs) const assumptious
         -> difference_type {
            LglsAssumeUser(mRange == rhs.mRange,
               "Iterators are for different containers");

            if constexpr (CT::TypeErased<C>) {
               const auto range = mIt.template GetRawAs<uint8_t>() - rhs.mIt.template GetRawAs<uint8_t>();
               return static_cast<difference_type>(range / mRange->GetStride());
            }
            else {
               const auto range = mIt.GetRaw() - rhs.mIt.GetRaw();
               return static_cast<difference_type>(range);
            }
         }
      };

      constexpr auto begin() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return {range.template AsAt<H>(range.GetCount() - 1), &range};
         else
            return {range.template AsAt<H>(0), &range};
      }

      constexpr auto end() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return --Iterator{range.template AsAt<H>(0), &range};
         else
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), &range};
      }

      constexpr auto rbegin() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return {range.template AsAt<H>(0), &range};
         else
            return {range.template AsAt<H>(range.GetCount() - 1), &range};
      }

      constexpr auto rend() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), &range};
         else
            return --Iterator{range.template AsAt<H>(0), &range};
      }
   };

   template<class C>
   IterateDefault(C&) -> IterateDefault<false, C>;
}