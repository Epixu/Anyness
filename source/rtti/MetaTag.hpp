#pragma once
#include "Meta.hpp"
#include "Intent.hpp"


namespace Langulus::RTTI
{
   class DefinitionTag;

   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      struct MetaTagPacked_16 : MetaPacked<2> {

      };
   #endif
      
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaTagNaked : MetaNaked<DefinitionTag> {
         using MetaNaked<DefinitionTag>::MetaNaked;
         using MetaNaked<DefinitionTag>::operator =;
         using MetaNaked<DefinitionTag>::operator bool;

         template<class, class...>
         bool IsExact() const noexcept;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaTagBase = MetaTagPacked_16;
   #else
      using MetaTagBase = MetaTagNaked;
   #endif

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Tag ID                                                               
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaTag : Inner::MetaTagBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaTag);

      using Inner::MetaTagBase::MetaTagBase;
      using Inner::MetaTagBase::operator =;
      using Inner::MetaTagBase::operator bool;
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI