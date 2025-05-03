#pragma once
#include <Langulus/CTTI.hpp>


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Iterator<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Iterator = Yes/No;` in T                   
   template<class T>
   struct Iterator {
      static constexpr bool Enabled = false;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Iterator);

namespace Langulus::Anyness
{
   
   ///                                                                        
   ///   A weightless 'end' iterator helper type                              
   ///                                                                        
   /// Used to return from container's end() methods. It only compares        
   /// equal to other iterators, if they've reached their end marker          
   ///                                                                        
   struct IteratorEnd {
      using CTTI_Iterator = Yes;
      using CTTI_ReflectAs = void;
   };


   ///                                                                        
   ///   Iterator                                                             
   ///                                                                        
   template<CT::Container C>
   struct TIterator {
      static_assert(CT::NotReference<C> and CT::NoIntent<C>);
      static constexpr bool Mutable = CT::Mutable<C>;
      static constexpr bool TypeErased = C::TypeErased;

      using Type = Tif<Mutable, TypeOf<C>, const TypeOf<C>>;
      using CTTI_Typed = Type;
      using CTTI_Iterator = Yes;
      using CTTI_ReflectAs = void;

   protected:
      using TypeInner = Tif<TypeErased, C, Type*>;

      // Current iterator position pointer                              
      TypeInner mValue = nullptr;
      // Iterator position which is considered the 'end' iterator       
      Type const* mEnd = nullptr;

      constexpr TIterator(const TypeInner&, Type const*) noexcept;

   public:
      constexpr TIterator() noexcept = default;
      constexpr TIterator(const TIterator&) noexcept = default;
      constexpr TIterator(TIterator&&) noexcept = default;
      constexpr TIterator(IteratorEnd) noexcept;

      constexpr auto operator = (const TIterator&) noexcept -> TIterator& = default;
      constexpr auto operator = (TIterator&&) noexcept -> TIterator& = default;

      constexpr bool operator == (const TIterator&) const noexcept;
      constexpr bool operator == (IteratorEnd) const noexcept;

      constexpr decltype(auto) operator * () const noexcept {
         if constexpr (not TypeErased) return *mValue;
         else return (mValue);
      }

      constexpr decltype(auto) operator -> () const noexcept {
         if constexpr (not TypeErased) return *mValue;
         else return &mValue;
      }

      // Prefix operator                                                
      constexpr auto operator ++ () noexcept -> TIterator&;

      // Suffix operator                                                
      constexpr auto operator ++ (int) noexcept -> TIterator;

      constexpr explicit operator bool() const noexcept;

      // Implicit cast to a constant iterator                           
      constexpr operator TIterator<const C>() const noexcept requires Mutable {
         return {mValue, mEnd};
      }
   };

} // namespace Langulus::Anyness