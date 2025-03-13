#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type                    
   ///   @tparam T    - the type of the variable                              
   ///   @tparam TYPE - optionally static type, use void for type-erasure     
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class T, class TYPE = void, unsigned ID = 0>
   struct TypedStack {
   private:
      // The type                                                       
      T mType;

   public:
      using CTTI_Component = Yes;
      using CTTI_Typed     = TYPE;
      static constexpr bool TypeErased = CT::Void<TYPE>;

      constexpr T GetType() const noexcept { return mType; }

      template<CT::Data, CT::Data...>
      constexpr bool Is() const noexcept;
      bool Is(DMeta) const noexcept;
      bool Is(const CT::Block auto&) const noexcept;

      template<CT::Data, CT::Data...>
      constexpr bool IsSimilar() const noexcept;
      bool IsSimilar(DMeta) const noexcept;
      bool IsSimilar(const CT::Block auto&) const noexcept;

      template<CT::Data, CT::Data...>
      constexpr bool IsExact() const noexcept;
      bool IsExact(DMeta) const noexcept;
      bool IsExact(const CT::Block auto&) const noexcept;

      template<bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsToMeta(DMeta) const;
      template<bool BINARY_COMPATIBLE = false>
      bool CastsToMeta(DMeta, Count) const;

      template<CT::Data, bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsTo() const;
      template<CT::Data, bool BINARY_COMPATIBLE = false>
      bool CastsTo(Count) const;

      template<bool CONSTRAIN = false>
      void SetType(DMeta) requires TypeErased;
      template<CT::Data, bool CONSTRAIN = false>
      void SetType() requires TypeErased;

      template<CT::Container C>
      constexpr decltype(auto) Get(this C&& self) {

      }
   };

} // namespace Langulus::Anyness::Component
