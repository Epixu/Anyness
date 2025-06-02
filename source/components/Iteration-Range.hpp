#pragma once
#include "../Container.hpp"
#include "../Iterator.hpp"
#include <ranges>
#include <tuple>


namespace Langulus::Anyness
{

   ///                                                                        
   ///   A weightless 'end' iterator helper type                              
   ///                                                                        
   /// Used to return from container's end() methods. It only compares        
   /// equal to other iterators if they have reached their end marker         
   ///                                                                        
   struct IteratorEnd final {
      using CTTI_Iterator = Yes;
      using CTTI_ReflectAs = void;
   };

   ///                                                                        
   ///   Reverse iteration adapter                                            
   ///                                                                        
   /// Use like this: for(auto i : IterateInReverse(container)), where        
   /// 'container' can be any range, including a std one                      
   ///                                                                        
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
   ///   Iterate multiple containers with the same ranged-for                 
   ///                                                                        
   /// Use like this: for(auto i : IterateTogether(pack1, pack2)), where      
   /// 'packN' can be any range, including a std one. You can retrieve the    
   /// current element by using i[N], or i.one() i.two() for the first two.   
   ///                                                                        
   template<::std::ranges::range...C>
   struct IterateTogether {
      static_assert(sizeof...(C) > 1,
         "IterateTogether needs at least two containers");

      using CTTI_ReflectAs = void;
      ::std::tuple<C&...> range;

      explicit constexpr IterateTogether(C&...a) noexcept : range {a...} {}

      /// A single combined iterator                                          
      struct Iterator {
         using CTTI_Iterator = Yes;
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
         /*explicit*/ constexpr Iterator(const T& it) noexcept : mIt {it} {}

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
      };

   public:
      Iterator begin() {
         return ::std::apply([](auto&...i) { return Iterator {{i.begin()...}}; }, range);
      }

      Iterator end() {
         return ::std::apply([](auto&...i) { return Iterator {{i.end()...}}; }, range);
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
      using CTTI_ReflectAs = void;

      C& range;

      explicit constexpr IterateNoDeref(C& a) noexcept : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator = Yes;
         using CTTI_ReflectAs = void;

      protected:
         using T = decltype(Fake<C>().begin());
         T mIt;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         /*explicit*/ constexpr Iterator(const T& it) noexcept : mIt {it} {}

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
      };

   public:
      Iterator       begin() { return Iterator {range.begin()}; }
      decltype(auto) end  () { return range.end(); }
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
      static_assert(CT::NoIntent<C>, "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");

      using CTTI_ReflectAs = void;

      C& range;

      explicit constexpr IterateHandles(C& a) noexcept : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator = Yes;
         using CTTI_ReflectAs = void;

      protected:
         using H = decltype(Fake<C>().GetHandle());
         mutable H mIt;
         C const& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         /*explicit*/ constexpr Iterator(H&& it, const C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (const Iterator& rhs) const noexcept {
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEnd();
         }

         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return {mIt++, mRange}; }
      };

   public:
      constexpr Iterator    begin() const noexcept { return {range.GetHandle(), range}; }
      constexpr IteratorEnd end  () const noexcept { return {}; }
   };

   template<CT::Container C>
   IterateHandles(C&) -> IterateHandles<C>;


   ///                                                                        
   ///   Default iteration                                                    
   ///                                                                        
   /// Used by default when doing for(auto i : container)                     
   /// When container is type-erased, or mutable and sparse, 'i' will be a    
   /// handle. Otherwise, 'i' will be a direct reference to the element       
   ///                                                                        
   template<CT::Container C>
   struct IterateDefault {
      static_assert(CT::NoIntent<C>, "C can't have an intent");
      static_assert(CT::NotReference<C>, "C can't be a reference");
      using CTTI_ReflectAs = void;

      static constexpr bool UsingHandles = CT::Untyped<C> or (CT::Mutable<C> and C::Sparse);

      C& range;

      explicit constexpr IterateDefault(C& a) noexcept : range {a} {}

      /// The iterator                                                        
      struct Iterator {
         using CTTI_Iterator = Yes;
         using CTTI_ReflectAs = void;

      protected:
         using H = Tif<UsingHandles,
            decltype(Fake<C>().GetHandle()),
            Tif<CT::Mutable<C>, TypeOf<C>*, TypeOf<C> const*>
         >;

         mutable H mIt;
         C const& mRange;

      public:
         Iterator() = delete;
         constexpr Iterator(Iterator const&) noexcept = default;
         constexpr Iterator(Iterator&&) noexcept = default;
         /*explicit*/ constexpr Iterator(H&& it, const C& range) noexcept
            : mIt    {FWD(it)}
            , mRange {range} {}

         constexpr bool operator == (const Iterator& rhs) const noexcept {
            return mIt.GetRaw() == rhs.mIt.GetRaw();
         }

         constexpr bool operator == (const IteratorEnd&) const noexcept {
            return mIt.GetRaw() == mRange.GetRawEnd();
         }

         H& operator *  () const noexcept { return  mIt; }
         H* operator -> () const noexcept { return &mIt; }

         Iterator& operator ++ ()    noexcept { ++mIt; return *this; }
         Iterator  operator ++ (int) noexcept { return {mIt++, mRange}; }
      };

   public:
      constexpr Iterator       begin() const noexcept { return Iterator {range.begin()}; }
      constexpr decltype(auto) end  () const noexcept { return range.end(); }
   };

   template<CT::Container C>
   IterateDefault(C&) -> IterateDefault<C>;

} // namespace Langulus::Anyness


namespace Langulus::Anyness::Component
{

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

      template<CT::Container C>
      using Iterator = typename IterateDefault<Deref<C>>::Iterator;

      template<CT::Container C>
      using IteratorRev = typename IterateInReverse<Deref<C>>::Iterator;

   public:
      /// Return an iterator to the first element                             
      template<CT::Container C>
      constexpr auto begin(this C&& self) noexcept -> Iterator<C> {
         if constexpr (CT::Untyped<C> or (CT::Mutable<C> and Deref<C>::Sparse))
            return {self.GetHandle(), self};
         else
            return {self.GetRaw(), self};
      }


      /// Return the last item                                                
      template<CT::Container C>
      constexpr auto last(this C&& self) noexcept -> Iterator<C> {
         const auto offset = self.IsEmpty() ? 0 : self.GetCount() - 1;

         if constexpr (CT::Untyped<C> or (CT::Mutable<C> and Deref<C>::Sparse))
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

} // namespace Langulus::Anyness::Component
