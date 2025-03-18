#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{
   class DefinitionConst;
   
   namespace Inner
   {
      
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaConstNaked {
      private:
         const DefinitionConst* mDefinition;

      public:
         template<class, class...>
         bool IsExact() const noexcept;
         bool IsExact(const MetaConstNaked&) const noexcept;

         /// Compare if two tags match exactly                                
         bool operator == (const MetaConstNaked& rhs) const noexcept {
            return IsExact(rhs);
         }
      };

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Constant ID                                                          
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   /// - all this is configurable.                                            
   ///                                                                        
   struct MetaConst
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      : Inner::MetaPacked<2>
   #else
      : Inner::MetaConstNaked
   #endif
   {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      constexpr MetaConst() noexcept = default;
      constexpr MetaConst(::std::nullptr_t) noexcept {}
      constexpr MetaConst(const DefinitionConst*) noexcept;
   };

   using CMeta = MetaConst;

} // namespace Langulus::RTTI