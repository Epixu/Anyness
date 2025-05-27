#pragma once
#include "../Container.hpp"
#include "../Iterator.hpp"
#include <Langulus/Sequence.hpp>
#include <ranges>
#include <tuple>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : IterateInReverse(container)), where        
   /// 'container' can be any range, including a std one                      
   ///                                                                        
   template<::std::ranges::range C>
   struct IterateInReverse {
      C& range;

      IterateInReverse(C& a) : range {a} {}

      auto begin()  { return range.rbegin(); }
      auto end()    { return range.rend();   }
   };

   template<::std::ranges::range C>
   IterateInReverse(C&) -> IterateInReverse<C>;


   ///                                                                        
   ///   Iterate multiple containers with the same ranged-for                 
   ///                                                                        
   /// Use like this: for(auto i : IterateTogether(pack1, pack2)), where      
   /// 'packN' can be any range, including a std one. You can retrieve the    
   /// current element by using i[N], or i.one() i.two() for the first two.   
   ///                                                                        
   template<::std::ranges::range...C>
   struct IterateTogether {
      static_assert(sizeof...(C) > 1, "IterateTogether needs at least two containers");
      ::std::tuple<C&...> range;

      IterateTogether(C&...a) : range {a...} {}

      struct WrapBegin {
      protected:
         using Type = ::std::tuple<decltype(Fake<C>().begin())...>;
         Type mIt;

      public:
         auto& one() { return ::std::get<0>(mIt); }
         auto& two() { return ::std::get<2>(mIt); }

         WrapBegin(const Type& it) : mIt {it} {}

         bool operator == (const WrapBegin& rhs) const noexcept {
            return mIt == rhs.mIt;
         }
         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }

         WrapBegin& operator *  () const noexcept { return *this; }
         WrapBegin& operator -> () const noexcept { return *this; }

         WrapBegin& operator ++ ()    noexcept { ++mIt; return *this; }
         WrapBegin  operator ++ (int) noexcept { return mIt++; }
      };

   public:
      WrapBegin begin() {
         return LANGULUS_SEQUENCE(sizeof...(C), {
            return WrapBegin {{::std::get<I>(range).begin()...}};
         });
      }

      WrapBegin end() {
         return LANGULUS_SEQUENCE(sizeof...(C), {
            return WrapBegin {{::std::get<I>(range).end()...}};
         });
      }
   };
   
   template<::std::ranges::range...C>
   IterateTogether(C&...) -> IterateTogether<C...>;


   ///                                                                        
   ///   Keep iterator when using ranged-for                                  
   ///                                                                        
   /// When doing for(auto i : container), the statement always               
   /// dereferences the iterator and 'i' always ends up with the contained    
   /// type - counteract this, and make 'i' be the iterator type instead      
   /// Use like this: for(auto i : IterateNoDeref(container)), where          
   /// 'container' can be any range, including a std one                      
   ///                                                                        
   template<::std::ranges::range C>
   struct IterateNoDeref {
      C& range;

      IterateNoDeref(C& a) : range {a} {}

      struct WrapBegin {
      protected:
         using Type = decltype(Fake<C>().begin());
         Type mIt;

      public:
         WrapBegin(const Type& it) : mIt {it} {}

         bool operator == (const WrapBegin& rhs) const noexcept {
            return mIt == rhs.mIt;
         }
         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }

         Type& operator *  () const noexcept { return mIt; }
         Type& operator -> () const noexcept { return mIt; }

         WrapBegin& operator ++ ()    noexcept { ++mIt; return *this; }
         WrapBegin  operator ++ (int) noexcept { return mIt++; }
      };

   public:
      auto begin() { return WrapBegin {range.begin()}; }
      auto end  () { return range.end(); }
   };

   template<::std::ranges::range C>
   IterateNoDeref(C&) -> IterateNoDeref<C>;


   ///                                                                        
   ///   Iterate using handles                                                
   ///                                                                        
   /// When doing for(auto i : container), the statement always uses the most 
   /// optimal iteration approach, but often you want to be able to modify    
   /// values in-place while iterating.                                       
   /// Use like this: for(auto i : IterateHandles(container)), where          
   /// 'container' can be any anyness container                               
   ///                                                                        
   template<CT::Container C>
   struct IterateHandles {
      using H = decltype(Fake<C>().GetHandle());

      C& range;

      explicit IterateHandles(C& a) : range {a} {}

      struct WrapBegin {
      protected:
         H  mIt;
         C& mRange;

      public:
         constexpr WrapBegin(C& range) noexcept
            : mIt    {range.GetHandle()}
            , mRange {range} {}

         constexpr bool operator == (const WrapBegin& rhs) const noexcept {
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEnd();
         }

         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         WrapBegin& operator ++ ()    noexcept { ++mIt; return *this; }
         WrapBegin  operator ++ (int) noexcept { return mIt++; }
      };

   public:
      constexpr WrapBegin   begin() const noexcept { return {range}; }
      constexpr IteratorEnd end  () const noexcept { return {};      }
   };

   template<CT::Container C>
   IterateHandles(C&) -> IterateHandles<C>;


   ///                                                                        
   /// Implements ranged iteration interface for containers                   
   ///   @tparam ID - heap/stack we're iterating                              
   ///                                                                        
   template<unsigned ID = 0>
   struct IterationRange {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      
   public:
      template<CT::Container C>
      constexpr auto begin(this C&& self) noexcept -> TIterator<Deref<C>>;

      template<CT::Container C>
      constexpr auto last(this C&& self) noexcept -> TIterator<Deref<C>>;

      constexpr IteratorEnd end() const noexcept { return {}; }
   };

} // namespace Langulus::Anyness::Component
