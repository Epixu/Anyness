#pragma once
#include <Langulus/CTTI.hpp>
#include <Langulus/CT/Contiguous.hpp>


namespace Langulus::Anyness
{
   


   ///                                                                        
   ///   Contiguous iterator                                                  
   ///                                                                        
   /*template<CT::Container C>
   struct TIterator {
      static_assert(CT::NotReference<C> and CT::NoIntent<C>,
         "Can't iterate intents or references");
      //static_assert(CT::Contiguous<C>, //TODO circular concept, maybe move it in destructor?
      //   "This iterator is designed for contiguous containers");

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
      constexpr TIterator(TIterator const&) noexcept = default;
      constexpr TIterator(TIterator&&) noexcept = default;
      constexpr TIterator(IteratorEnd) noexcept;

      constexpr auto operator = (TIterator const&) noexcept -> TIterator& = default;
      constexpr auto operator = (TIterator&&) noexcept -> TIterator& = default;

      constexpr bool operator == (TIterator const&) const noexcept;
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

   
   ///                                                                        
   ///   Map iterator                                                         
   ///                                                                        
   template<CT::Container C>
   struct TIteratorMap {
      static_assert(CT::NotReference<C> and CT::NoIntent<C>,
         "Can't iterate intents or references");
      static_assert(not CT::Contiguous<C> and CT::Map<C>,
         "This iterator is designed for non-contiguous map containers");

      static constexpr bool Mutable = CT::Mutable<C>;
      static constexpr bool TypeErased = C::TypeErased;

      // Key type is always constant, because changing it will mean     
      // rehashing the entire table, so we forbid it while iterating    
      using Key   = typename C::Key;
      using Val   = Tif<Mutable, typename C::ValMut, typename C::Val>;
      using KA    = Tif<CT::Reference<Key>, Deref<Key>*, Key>;
      using VA    = Tif<CT::Reference<Val>, Deref<Val>*, Val>;
      using Table = typename C::TableType;

      using CTTI_Typed = Types<Key, Val>;
      using CTTI_Iterator = Yes;
      using CTTI_ReflectAs = void;

   protected:
      KA mKey;
      VA mVal;

      friend struct BlockMap;
      const Table* mInfo = nullptr;
      const Table* mEnd  = nullptr;

      constexpr TIteratorMap(const Table*, const Table*, const KA&, const VA&) noexcept;

   public:
      constexpr TIteratorMap() noexcept = default;
      constexpr TIteratorMap(TIteratorMap const&) noexcept = default;
      constexpr TIteratorMap(TIteratorMap&&) noexcept = default;
      constexpr TIteratorMap(IteratorEnd) noexcept;

      auto& GetKey(this auto&& self) noexcept {
         if constexpr (CT::Reference<Key>)
            return *self.mKey;
         else
            return  self.mKey;
      }

      auto& GetVal(this auto&& self) noexcept {
         if constexpr (CT::Reference<Key>)
            return *self.mVal;
         else
            return  self.mVal;
      }
      
      constexpr auto operator = (TIteratorMap const&) noexcept -> TIteratorMap& = default;
      constexpr auto operator = (TIteratorMap&&)      noexcept -> TIteratorMap& = default;

      constexpr bool operator == (const TIteratorMap&) const noexcept;
      constexpr bool operator == (const IteratorEnd&) const noexcept;

      constexpr auto operator *  () const;
      constexpr auto operator -> () const noexcept { return &GetVal(); }

      // Prefix operator                                                
      constexpr auto operator ++ () noexcept -> TIteratorMap&;

      // Suffix operator                                                
      constexpr auto operator ++ (int) noexcept -> TIteratorMap;

      constexpr explicit operator bool() const noexcept;

      constexpr operator TIteratorMap<const C>() const noexcept requires Mutable {
         return {mInfo, mEnd, mKey, mVal};
      }
   };*/

} // namespace Langulus::Anyness