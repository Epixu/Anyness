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
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : IterateInReverse(container)), where        
   /// 'container' can be any range, including std one.                       
   template<class C>
   struct IterateInReverse {
      using CTTI_ReflectAs = void;
      static_assert(CT::NoIntent<C>,         "C can't have an intent");
      static_assert(CT::NotReference<C>,     "C can't be a reference");
      static_assert(::std::ranges::range<C>, "C is not a range");

      C& range;

      constexpr IterateInReverse(C& a) noexcept
         : range {a} {}

      decltype(auto) begin() noexcept { return range.rbegin(); }
      decltype(auto) end()   noexcept { return range.rend();   }
   };

   template<class C>
   IterateInReverse(C&) -> IterateInReverse<C>;


   ///                                                                        
   ///   Keep iterator when using ranged-for                                  
   ///                                                                        
   /// When doing for(auto i : container), the statement always dereferences  
   /// the iterator and 'i' always ends up with the contained type.           
   /// Counteract this, and make 'i' be the iterator type instead.            
   /// Use like this: for(auto i : IterateNoDeref(container)), where          
   /// 'container' can be any range, including std one                        
   template<bool REVERSE, class C>
   struct IterateNoDeref {
      using CTTI_ReflectAs = void;
      static_assert(CT::NoIntent<C>,
         "C can't have an intent");
      static_assert(CT::NotReference<C>,
         "C can't be a reference");
      static_assert(::std::ranges::range<C>,
         "C is not a range");

   protected:
      using Count = typename Deref<C>::CountType;
      using H = Tif<REVERSE, decltype(Fake<C>().rbegin()),
                             decltype(Fake<C>().begin())>;

      static_assert(CT::NotReference<H>,
         "Iterator can't be a reference");
      static_assert(CT::Iterator<H>,
         "Must be an iterator");

      C& range;

   public:
      constexpr IterateNoDeref(C& a) noexcept
         : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_ReflectAs    = void;
         using CTTI_Iterator     = Yes<>;
         using difference_type   = std::ptrdiff_t;
         using iterator_category = typename C::IteratorCategory;
         using value_type        = H;
         using reference         = H&;

         H mIt;
         C& mRange;

         constexpr Iterator() noexcept = default;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H const& it, C& range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C& range) noexcept
            : mIt    {LglsFwd(it)}
            , mRange {range} {}

         constexpr auto operator = (Iterator const& rhs) assumptious -> Iterator& {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr auto operator = (Iterator&& rhs) assumptious -> Iterator& {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr bool operator == (CT::Iterator auto const& rhs) const assumptious {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            return mIt == rhs.mIt;
         }

         constexpr auto operator <=> (CT::Iterator auto const& rhs) const noexcept {
            return mIt <=> rhs.mIt;
         }

         explicit constexpr operator bool() const noexcept {
            if constexpr (REVERSE) return mIt != mRange.rend();
            else                   return mIt != mRange.end();
         }

         decltype(auto) operator *  () const noexcept { return (mIt); /* *mIt;*/   }
         decltype(auto) operator -> () const noexcept { return &(*mIt); }

         
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

      auto begin() -> Iterator  {
         if constexpr (REVERSE) return {range.rbegin(), range};
         else                   return {range.begin(),  range};
      }

      auto end() -> Iterator {
         if constexpr (REVERSE) return {range.rend(), range};
         else                   return {range.end(),  range};
      }
   };

   template<class C>
   IterateNoDeref(C&) -> IterateNoDeref<false, C>;


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
      using Pick    = typename C::Pick;
      using PickMut = typename C::PickMut;
      using Count   = typename Deref<C>::CountType;

      // The handle is either a pointer/THandle for statically-typed    
      // containers, or Handle/HandleMut for type-erased ones           
      using H = Tmut<C,
         Tif<CT::Handle<PickMut>,   PickMut,          Deref<PickMut>*>,
         Tif<CT::Handle<Pick>,      Pick,    ConstAll<Deref<Pick>*>>
      >;

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
            return {range.template As<H>(), &range};
      }

      constexpr auto end() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return --Iterator{range.template As<H>(), &range};
         else
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), &range};
      }

      constexpr auto rbegin() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return {range.template As<H>(), &range};
         else
            return {range.template AsAt<H>(range.GetCount() - 1), &range};
      }

      constexpr auto rend() noexcept -> Iterator {
         if (range.IsEmpty())
            return {{}, &range};

         if constexpr (REVERSE)
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), &range};
         else
            return --Iterator{range.template As<H>(), &range};
      }
   };

   template<class C>
   IterateDefault(C&) -> IterateDefault<false, C>;


   ///                                                                        
   ///   Iterate using handles                                                
   ///                                                                        
   /// When doing for(auto i : container), the statement always uses the most 
   /// optimal iteration approach, but often you want to be able to modify    
   /// values in-place while iterating.                                       
   /// Use like this: for(auto i : IterateHandles(container)), where          
   /// 'container' can be any CT::Container.                                  
   template<bool REVERSE, class C>
   struct IterateHandles {
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
      using Count = typename Deref<C>::CountType;
      using H     = DecideHandle<C>;

      static_assert(CT::NotReference<H>,
         "Iterator can't be a reference");
      static_assert(CT::Handle<H>,
         "Iterator must always be a handle");

      C& range;

   public:
      explicit constexpr IterateHandles(C& a) noexcept
         : range{a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_ReflectAs = void;
         using CTTI_Iterator  = Yes<>;
         using difference_type = std::ptrdiff_t;
         using value_type = H;
         using reference = H&;

         H mIt;
         C& mRange;

         constexpr Iterator() noexcept = default;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(H const& it, C& range) noexcept
            : mIt    {it}
            , mRange {range} {}
         constexpr Iterator(H&& it, C& range) noexcept
            : mIt    {LglsFwd(it)}
            , mRange {range} {}

         constexpr auto operator = (Iterator const& rhs) assumptious -> Iterator& {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr auto operator = (Iterator&& rhs) assumptious -> Iterator& {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            mIt = rhs.mIt;
            return *this;
         }

         constexpr bool operator == (CT::Iterator auto const& rhs) const assumptious {
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr auto operator <=> (CT::Iterator auto const& rhs) const noexcept {
            return mIt.GetRaw() <=> rhs.mIt.GetRaw();
         }

         explicit constexpr operator bool() const noexcept {
            return mIt.GetRaw() != mRange.GetRawEnd();
         }
         
         decltype(auto) operator *  ()       noexcept { return (mIt); }
         decltype(auto) operator *  () const noexcept { return (mIt); }
         decltype(auto) operator -> ()       noexcept { return &mIt; }
         decltype(auto) operator -> () const noexcept { return &mIt; }

         auto operator + (Count c) const noexcept -> Iterator {
            if constexpr (REVERSE) return {mIt - c, mRange};
            else                   return {mIt + c, mRange};
         }

         auto operator - (Count c) const noexcept -> Iterator {
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
            if constexpr (REVERSE) return mIt - offset;
            else                   return mIt + offset;
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
            LglsAssumeUser(&mRange == &rhs.mRange,
               "Iterators are for different containers");

            if constexpr (CT::TypeErased<C>) {
               const auto range = mIt.template GetRawAs<uint8_t>() - rhs.mIt.template GetRawAs<uint8_t>();
               return static_cast<difference_type>(range / mRange.GetStride());
            }
            else {
               const auto range = mIt.GetRaw() - rhs.mIt.GetRaw();
               return static_cast<difference_type>(range);
            }
         }
      };

      constexpr Iterator begin() const noexcept {
         if (range.IsEmpty()) {
            if constexpr (CT::TypeErased<H>) {
               if constexpr (CT::DeeplyOwned<H>)
                  return {H {range.Get(), nullptr, {}}, range};
               else if constexpr (CT::Owned<H>)
                  return {H {range.Get(), nullptr, {}}, range};
               else
                  return {H {range.Get(), {}}, range};
            }
            else {
               if constexpr (CT::DeeplyOwned<H>)
                  return {H {&range.Get(), nullptr}, range};
               else if constexpr (CT::Owned<H>)
                  return {H {&range.Get(), nullptr}, range};
               else
                  return {H {&range.Get()}, range};
            }
         }

         if constexpr (REVERSE)
            return {range.template AsAt<H>(range.GetCount() - 1), range};
         else
            return {range.template As<H>(), range};
      }

      constexpr Iterator end() const noexcept {
         if (range.IsEmpty()) {
            if constexpr (CT::TypeErased<H>) {
               if constexpr (CT::DeeplyOwned<H>)
                  return {H {range.Get(), nullptr, {}}, range};
               else if constexpr (CT::Owned<H>)
                  return {H {range.Get(), nullptr, {}}, range};
               else
                  return {H {range.Get(), {}}, range};
            }
            else {
               if constexpr (CT::DeeplyOwned<H>)
                  return {H {&range.Get(), nullptr}, range};
               else if constexpr (CT::Owned<H>)
                  return {H {&range.Get(), nullptr}, range};
               else
                  return {H {&range.Get()}, range};
            }
         }

         if constexpr (REVERSE)
            return --Iterator{range.template As<H>(), range};
         else
            return ++Iterator{range.template AsAt<H>(range.GetCount() - 1), range};
      }
   };

   template<class C>
   IterateHandles(C&) -> IterateHandles<false, C>;


   ///                                                                        
   ///   Iterate multiple containers with the same ranged-for                 
   ///                                                                        
   /// Use like this: for(auto i : IterateTogether(pack1, pack2)), where      
   /// 'packN' can be any range, including std one. You can retrieve the      
   /// current element by using i[N], or i.one() i.two() for the first two.   
   template<bool REVERSE, class...C>
   struct IterateTogether {
      using CTTI_ReflectAs = void;
      static_assert(CT::NoIntent<C...>,
         "C can't have an intent");
      static_assert(CT::NotReference<C...>,
         "C can't be a reference");
      static_assert((::std::ranges::range<C> and ...),
         "C is not a range");
      static constexpr size_t Size = sizeof...(C);
      static_assert(Size > 1,
         "IterateTogether needs at least two containers");

   protected:
      //using Count = size_t;
      using Hs = ::std::tuple<Tif<REVERSE, decltype(Fake<C>().rbegin()),
                                           decltype(Fake<C>().begin())>...>;
      using Cs = ::std::tuple<C&...>;

      Cs ranges;

   public:
      explicit constexpr IterateTogether(C&...a) noexcept
         : ranges {a...} {}

      /// A single combined iterator                                          
      struct Iterator {
         using CTTI_ReflectAs = void;
         using CTTI_Iterator = Yes<>;
         using difference_type = std::ptrdiff_t;

         Hs mIt;
         Cs mRanges;

         template<size_t I>
         constexpr auto* Range() const noexcept {
            return &::std::get<I>(mRanges);
         }

         template<size_t I>
         constexpr auto* Get() const noexcept {
            decltype(auto) it = *::std::get<I>(mIt);
            if constexpr (CT::Handle<decltype(it)>)
               return it.GetRaw();
            else
               return &it;
         }

         decltype(auto) one() noexcept { return *::std::get<0>(mIt); }
         decltype(auto) two() noexcept { return *::std::get<1>(mIt); }

         constexpr Iterator() noexcept = default;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         constexpr Iterator(Hs const& it, Cs& ranges) noexcept
            : mIt    {it}
            , mRanges{ranges} {}
         constexpr Iterator(Hs&& it, Cs& ranges) noexcept
            : mIt    {LglsFwd(it)}
            , mRanges{ranges} {}

         constexpr auto operator = (Iterator const& rhs) noexcept -> Iterator& {
            mIt = rhs.mIt;
            return *this;
         }

         constexpr auto operator = (Iterator&& rhs) noexcept -> Iterator& {
            mIt = rhs.mIt;
            return *this;
         }

         constexpr bool operator == (CT::Iterator auto const& rhs) const assumptious {
            return LglsSequence(Size, {
               LglsAssumeUser(((Range<I>() == rhs.template Range<I>()) and ...),
                  "Iterators are for different containers");
               return ((Get<I>() == rhs.template Get<I>()) and ...);
            });
         }

         constexpr bool operator < (CT::Iterator auto const& rhs) const assumptious {
            return LglsSequence(Size, {
               LglsAssumeUser(((Range<I>() == rhs.template Range<I>()) and ...),
                  "Iterators are for different containers");
               return ((Get<I>() < rhs.template Get<I>()) and ...);
            });
         }

         constexpr bool operator <= (CT::Iterator auto const& rhs) const assumptious {
            return LglsSequence(Size, {
               LglsAssumeUser(((Range<I>() == rhs.template Range<I>()) and ...),
                  "Iterators are for different containers");
               return ((Get<I>() <= rhs.template Get<I>()) and ...);
            });
         }

         constexpr bool operator > (CT::Iterator auto const& rhs) const assumptious {
            return LglsSequence(Size, {
               LglsAssumeUser(((Range<I>() == rhs.template Range<I>()) and ...),
                  "Iterators are for different containers");
               return ((Get<I>() > rhs.template Get<I>()) and ...);
            });
         }

         constexpr bool operator >= (CT::Iterator auto const& rhs) const assumptious {
            return LglsSequence(Size, {
               LglsAssumeUser(((Range<I>() == rhs.template Range<I>()) and ...),
                  "Iterators are for different containers");
               return ((Get<I>() >= rhs.template Get<I>()) and ...);
            });
         }

         explicit constexpr operator bool() const noexcept {
            return LglsSequence(Size, {
               return ((Get<I>() != Range<I>().end()) and ...);
            });
         }

         auto operator *  ()       noexcept -> Iterator&       { return *this; }
         auto operator *  () const noexcept -> Iterator const& { return *this; }
         auto operator -> ()       noexcept -> Iterator&       { return *this; }
         auto operator -> () const noexcept -> Iterator const& { return *this; }
         
         friend auto operator + (difference_type lhs, Iterator const& rhs) noexcept -> Iterator {
            static_assert(not REVERSE);
            return LglsSequence(Size, {
               return Iterator(Hs{(::std::get<I>(rhs.mIt) + lhs)...}, rhs.mRanges);
            });
         }

         auto operator + (difference_type c) const noexcept -> Iterator {
            if constexpr (REVERSE) {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) + c)...}, mRanges);
               });
            }
            else {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) + c)...}, mRanges);
               });
            }
         }

         auto operator - (difference_type c) const noexcept -> Iterator {
            if constexpr (REVERSE) {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) - c)...}, mRanges);
               });
            }
            else {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) - c)...}, mRanges);
               });
            }
         }

         auto operator += (difference_type c) noexcept -> Iterator& {
            if constexpr (REVERSE)
               LglsSequence(Size, { ((::std::get<I>(mIt) -= c), ...); });
            else
               LglsSequence(Size, { ((::std::get<I>(mIt) += c), ...); });
            return *this;
         }

         auto operator -= (difference_type c) noexcept -> Iterator& {
            if constexpr (REVERSE)
               LglsSequence(Size, { ((::std::get<I>(mIt) += c), ...); });
            else
               LglsSequence(Size, { ((::std::get<I>(mIt) -= c), ...); });
            return *this;
         }

         decltype(auto) operator[] (const difference_type offset) const noexcept {
            if constexpr (REVERSE) {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) - offset)...}, mRanges);
               });
            }
            else {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt) + offset)...}, mRanges);
               });
            }
         }

         /// Prefix increment                                                 
         auto operator ++ () noexcept -> Iterator& {
            if constexpr (REVERSE)
               LglsSequence(Size, { ((--::std::get<I>(mIt)), ...); });
            else
               LglsSequence(Size, { ((++::std::get<I>(mIt)), ...); });
            return *this;
         }

         /// Suffix increment                                                 
         auto operator ++ (int) noexcept -> Iterator {
            if constexpr (REVERSE) {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt)--)...}, mRanges);
               });
            }
            else {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt)++)...}, mRanges);
               });
            }
         }

         /// Prefix decrement                                                 
         auto operator -- () noexcept -> Iterator& {
            if constexpr (REVERSE)
               LglsSequence(Size, { ((++::std::get<I>(mIt)), ...); });
            else
               LglsSequence(Size, { ((--::std::get<I>(mIt)), ...); });
            return *this;
         }

         /// Suffix decrement                                                 
         auto operator -- (int) noexcept -> Iterator {
            if constexpr (REVERSE) {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt)++)...}, mRanges);
               });
            }
            else {
               return LglsSequence(Size, {
                  return Iterator(Hs{(::std::get<I>(mIt)--)...}, mRanges);
               });
            }
         }

         /// Get the integer element difference between two iterators         
         auto operator - (CT::Iterator auto const& rhs) const assumptious
         -> difference_type {
            auto& lhs_range = ::std::get<0>(    mRanges);
            auto& rhs_range = ::std::get<0>(rhs.mRanges);
            LglsAssumeUser(&lhs_range == &rhs_range,
               "Iterators are for different containers");

            if constexpr (CT::TypeErased<decltype(lhs_range)>) {
               const auto range = one().template GetRawAs<uint8_t>() - rhs.one().template GetRawAs<uint8_t>();
               return static_cast<difference_type>(range / lhs_range.GetStride());
            }
            else {
               const auto range = one().GetRaw() - rhs.one().GetRaw();
               return static_cast<difference_type>(range);
            }
         }
      };

      auto begin() -> Iterator {
         return ::std::apply([&](auto&...i) {
            return Iterator{Hs{i.begin()...}, ranges};
         }, ranges);
      }

      auto end() -> Iterator {
         return ::std::apply([&](auto&...i) {
            return Iterator{Hs{i.end()...}, ranges};
         }, ranges);
      }
   };

   template<class...C>
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

      /// Return the end sentinel                                             
      template<CT::Container C>
      constexpr auto end(this C&& self) noexcept {
         return IterateDefault(self).end();
      }

      /// Return an iterator to the first element, reversed                   
      template<CT::Container C>
      constexpr auto rbegin(this C&& self) noexcept {
         return IterateDefault<true, Deref<C>>(self).begin();
      }

      /// Return the end sentinel                                             
      template<CT::Container C>
      constexpr auto rend(this C&& self) noexcept {
         return IterateDefault<true, Deref<C>>(self).end();
      }

      /// Return the last item                                                
      /*template<CT::Container C>
      constexpr auto last(this C&& self) noexcept -> Iterator<C> {
         const auto offset = self.IsEmpty() ? 0 : self.GetCount() - 1;

         if constexpr (CT::TypeErased<C> or (CT::Mutable<C> and Deref<C>::Sparse))
            return {self.GetHandle() + offset, self};
         else
            return {self.GetRaw() + offset, self};
      }*/
   };
}
