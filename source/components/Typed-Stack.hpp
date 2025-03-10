#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type                    
   ///   @tparam T    - the type of the variable                              
   ///   @tparam TYPE - optionally static type, use void for type-erasure     
   ///   @tparam ID   - multiple type variables are supported                 
   template<class T, class TYPE = void, unsigned ID = 0>
   struct TypedStack {
   private:
      // The type                                                       
      T mType;

   public:
      using CTTI_Component = Yes;
      using CTTI_Typed = TYPE;
      static constexpr bool TypeErased = CT::Void<TYPE>;

      constexpr T GetType() const noexcept { return mType; }

      template<CT::Container C>
      constexpr decltype(auto) Get(this C&& self) {

      }
   };

} // namespace Langulus::Anyness::Component
