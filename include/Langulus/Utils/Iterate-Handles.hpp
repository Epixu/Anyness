///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Iteration-Range.hpp"
#include <Langulus/Anyness/Handle.hpp>


namespace Langulus::Anyness
{
   ///                                                                        
   ///   Iterate using handles                                                
   ///                                                                        
   /// When doing for(auto i : container), the statement always uses the most 
   /// optimal iteration approach, but often you want to be able to modify    
   /// values in-place while iterating.                                       
   /// Use like this: for(auto i : IterateHandles(container)), where          
   /// 'container' can be any CT::Container.                                  
   template<CT::Container C>
   struct IterateHandles {
      using CTTI_ReflectAs = void;

      static_assert(CT::NoIntent<C>, "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      
      using H = DecideHandle<C>;

      C& range;

      explicit constexpr IterateHandles(C& a) noexcept : range {a} {}

   private:
      using Count = typename Deref<C>::CountType;

   public:
      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
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
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEnd();
         }

         explicit constexpr operator bool() const noexcept {
            return mIt.GetRaw() != mRange.GetRawEnd();
         }
         
         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         Iterator  operator + (Count c) const noexcept { return {mIt + c, mRange}; }
         Iterator  operator - (Count c) const noexcept { return {mIt - c, mRange}; }
         
         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return {mIt++, mRange}; }
         Iterator& operator -- ()    noexcept { --mIt; return *this; }
         Iterator  operator -- (int) noexcept { return {mIt--, mRange}; }

         /// Get the integer element difference between two iterators         
         Count operator - (const Iterator& rhs) const noexcept {
            if constexpr (CT::TypeErased<C>) {
               const auto range = mIt.template GetRawAs<uint8_t>() - rhs.mIt.template GetRawAs<uint8_t>();
               return static_cast<Count>(range / mRange.GetStride());
            }
            else {
               const auto range = mIt.GetRaw() - rhs.mIt.GetRaw();
               return static_cast<Count>(range);
            }
         }
      };

      constexpr Iterator begin() const noexcept {
         return {range.template As<H>(), range};
      }
      constexpr IteratorEnd end() const noexcept { return {}; }
   };

   template<CT::Container C>
   IterateHandles(C&) -> IterateHandles<C>;


   ///                                                                        
   ///   Iterate using handles in reverse                                     
   ///                                                                        
   /// When doing for(auto i : IterateInReverse(container)), the statement    
   /// always uses the most optimal iteration approach, but often you want to 
   /// be able to modify values in-place while iterating.                     
   /// Use like this: for(auto i : IterateHandlesInReverse(container)), where 
   /// 'container' can be any CT::Container.                                  
   template<CT::Container C>
   struct IterateHandlesInReverse {
      using CTTI_ReflectAs = void;

      static_assert(CT::NoIntent<C>, "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      
      using H = DecideHandle<C>;

      C& range;

      explicit constexpr IterateHandlesInReverse(C& a) noexcept : range {a} {}

   private:
      using Count = typename Deref<C>::CountType;

   public:
      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator  = Yes<>;
         using CTTI_ReflectAs = void;

      protected:
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
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEndRev();
         }

         explicit constexpr operator bool() const noexcept {
            return mIt.GetRaw() != mRange.GetRawEndRev();
         }
         
         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         Iterator  operator + (Count c) const noexcept { return {mIt - c, mRange}; }
         Iterator  operator - (Count c) const noexcept { return {mIt + c, mRange}; }
         
         Iterator& operator ++ ()    noexcept { --mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return {mIt--, mRange}; }
         Iterator& operator -- ()    noexcept { ++mIt; return *this; }
         Iterator  operator -- (int) noexcept { return {mIt++, mRange}; }

         /// Get the integer element difference between two iterators         
         Count operator - (const Iterator& rhs) const noexcept {
            if constexpr (CT::TypeErased<C>) {
               const auto range = mIt.template GetRawAs<uint8_t>() - rhs.mIt.template GetRawAs<uint8_t>();
               return static_cast<Count>(range / mRange.GetStride());
            }
            else {
               const auto range = mIt.GetRaw() - rhs.mIt.GetRaw();
               return static_cast<Count>(range);
            }
         }
      };

      constexpr Iterator begin() const noexcept {
         return {range.template AsAt<H>(range.GetCount() - 1), range};
      }
      constexpr IteratorEnd end() const noexcept { return {}; }
   };

   template<CT::Container C>
   IterateHandlesInReverse(C&) -> IterateHandlesInReverse<C>;
}
