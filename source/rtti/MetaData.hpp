///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Meta.hpp"
#include "DefinitionData.hpp"
#include <Langulus/HashOf.hpp>
#include <Langulus/IntentOf.hpp>


namespace Langulus::RTTI
{
   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      ///                                                                     
      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      ///                                                                     
      struct MetaDataStructured_8_8 : MetaPacked<DefinitionData, 1> {

      };

      struct MetaDataStructured_16_16 : MetaPacked<DefinitionData, 2> {

      };

      struct MetaDataStructured_24_8 : MetaPacked<DefinitionData, 3> {

      };

      struct MetaDataStructured_32_8 : MetaPacked<DefinitionData, 4> {

      };

      struct MetaDataStructured_32_16 : MetaPacked<DefinitionData, 4> {

      };
   #endif

      ///                                                                     
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      ///                                                                     
      struct MetaDataNaked : MetaNaked<DefinitionData> {
         using MetaNaked<DefinitionData>::MetaNaked;
         using MetaNaked<DefinitionData>::operator =;
         using MetaNaked<DefinitionData>::operator bool;

         bool Is(const MetaDataNaked&) const noexcept;
         bool IsSimilar(const MetaDataNaked&) const noexcept;

         auto GetMinAllocation()      const noexcept -> size_t;
         auto GetSize()               const noexcept -> size_t;
         auto GetAlignment()          const noexcept -> size_t;
         auto GetName()               const noexcept -> Token;
                                      
         bool IsDense()               const noexcept;
         bool IsSparse()              const noexcept;
         bool IsConstant()            const noexcept;
         bool IsMutable()             const noexcept;
         bool IsDeep()                const noexcept;
         bool IsPOD()                 const noexcept;

         auto GetDestructor()         const noexcept -> DefinitionData::FDestroy;
         auto GetReferencer()         const noexcept -> DefinitionData::FReference;
         auto GetResolver()           const noexcept -> DefinitionData::FResolve;
         auto GetReferConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetReferAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetMoveConstructor()    const noexcept -> DefinitionData::FMoveConstruct;
         auto GetMoveAssigner()       const noexcept -> DefinitionData::FMoveAssign;
         auto GetAbandonConstructor() const noexcept -> DefinitionData::FMoveConstruct;
         auto GetAbandonAssigner()    const noexcept -> DefinitionData::FMoveAssign;
         auto GetDisownConstructor()  const noexcept -> DefinitionData::FCopyConstruct;
         auto GetDisownAssigner()     const noexcept -> DefinitionData::FCopyAssign;
         auto GetCloneConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCloneAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetCopyConstructor()    const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCopyAssigner()       const noexcept -> DefinitionData::FCopyAssign;
         auto GetComparer()           const noexcept -> DefinitionData::FCompare;
         auto GetHasher()             const noexcept -> DefinitionData::FHash;
         bool HasGetHashMethod()      const noexcept;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaDataBase = MetaDataStructured_16_16;
   #else
      using MetaDataBase = MetaDataNaked;
   #endif

   } // namespace Langulus::RTTI::Inner
   

   ///                                                                        
   ///   Data type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaData : Inner::MetaDataBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaData);

      using Inner::MetaDataBase::MetaDataBase;
      using Inner::MetaDataBase::operator =;
      using Inner::MetaDataBase::operator bool;
   };

   using DMeta = MetaData;

} // namespace Langulus::RTTI

#include "MetaData.inl"