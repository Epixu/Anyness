#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   namespace Inner
   {

      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      struct MetaDataStructured_8_8 : MetaPacked<1> {

      };

      struct MetaDataStructured_16_16 : MetaPacked<2> {

      };

      struct MetaDataStructured_24_8 : MetaPacked<3> {

      };

      struct MetaDataStructured_32_8 : MetaPacked<4> {

      };

      struct MetaDataStructured_32_16 : MetaPacked<4> {

      };

   } // namespace Langulus::RTTI::Inner
#endif
    
   class DefinitionData;

   ///                                                                        
   ///   Data type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection - all this is configurable.      
   ///                                                                        
   struct MetaData
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      : Inner::MetaDataStructured_16_16
   #else
      : Inner::MetaNaked
   #endif
   {
      constexpr MetaData() noexcept = default;
      constexpr MetaData(::std::nullptr_t) noexcept {}
      constexpr MetaData(const DefinitionData*) noexcept;

      ::std::size_t GetMinAllocation() const noexcept;
   };

   using DMeta = MetaData;

} // namespace Langulus::RTTI