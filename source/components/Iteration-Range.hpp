#pragma once
#include "../Container.hpp"
#include "../Iterator.hpp"
#include <ranges>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : RangeReverse(container)), where            
   /// 'container' can be any range type                                      
   ///                                                                        
   template<::std::ranges::range C>
   struct RangeReverse {
      C& range;

      RangeReverse(C& a) : range {a} {}

      auto begin()  { return range.rbegin(); }
      auto end()    { return range.rend();   }
   };


   ///                                                                        
   ///   Keep iterator when using ranged-for                                  
   ///                                                                        
   /// When doing for(auto i : container), the statement always               
   /// dereferences the iterator and 'i' always ends up with the contained    
   /// type - counteract this, and make 'i' be the iterator type instead      
   /// Use like this: for(auto i : RangeIterator(container)), where           
   /// 'container' can be any range                                           
   ///                                                                        
   template<::std::ranges::range C>
   struct RangeIterator {
      C& range;

      RangeIterator(C& a) : range {a} {}

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

         decltype(auto) operator *  () const noexcept { return mIt; }
         decltype(auto) operator -> () const noexcept { return mIt; }

         WrapBegin& operator ++ ()    noexcept { ++mIt; return *this; }
         WrapBegin  operator ++ (int) noexcept { return mIt++; }
      };

   public:
      auto begin() { return WrapBegin {range.begin()}; }
      auto end  () { return range.end(); }
   };
   

   ///                                                                        
   ///   Iterate using handles                                                
   ///                                                                        
   /// When doing for(auto i : container), the statement always               
   /// uses the most optimal iteration approach, but often you want to be     
   /// able to modify values in-place while iterating.                        
   /// Use like this: for(auto i : RangeHandle(container)), where             
   /// 'container' can be any anyness container                               
   ///                                                                        
   template<CT::Container C>
   struct RangeHandle {
      C& range;

      RangeHandle(C& a) : range {a} {}

      struct WrapBegin {
      protected:
         using Type = decltype(Fake<C>().GetHandle());
         Type mIt;

      public:
         WrapBegin(const Type& it) : mIt {it} {}

         bool operator == (const WrapBegin& rhs) const noexcept {
            return mIt == rhs.mIt;
         }
         bool operator == (const IteratorEnd&) const noexcept {
            return mIt == IteratorEnd {};
         }

         decltype(auto) operator *  () const noexcept { return mIt; }
         decltype(auto) operator -> () const noexcept { return mIt; }

         WrapBegin& operator ++ ()    noexcept { ++mIt; return *this; }
         WrapBegin  operator ++ (int) noexcept { return mIt++; }
      };

   public:
      auto begin() { return WrapBegin {range.begin()}; }
      auto end  () { return range.end(); }
   };


   ///                                                                        
   /// Implements ranged iteration interface for containers                   
   ///                                                                        
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
