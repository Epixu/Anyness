///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/InfoOf.hpp>
#include <Langulus/Values.hpp>
#include <Langulus/CT/Members.hpp>

using namespace Langulus;

namespace Langulus::Tags
{
   struct Name {
      using CTTI_DefineTag = Yes<"Name">;
      Token name;
   };

   template<class T>
   struct TName {
      using CTTI_DefineTag = Yes<"Name">;
      T name;
   };
}

namespace Langulus::Flow
{
   struct Verb {};
}

namespace Langulus::Verbs
{
   template<class T>
   struct CreateIn {};
   
   struct Create {
      using CTTI_DefineVerb = VerbToken<"create", "destroy", 5.f>;
      using CTTI_DefineVerbOperator = VerbToken<" + ", " - ">;
      using CTTI_Info = Yes<
         "Used for allocating new elements. "
         "If the type you're creating has	a producer, "
         "you need to execute the verb in a matching producer, "
         "or that producer will be created automatically for you, if possible"
      >;
      
      template<class T>
      static constexpr bool IsAble
         = requires (T&& t, Flow::Verb& v) { t.Create(v); }
        or requires (Flow::Verb& v) { CreateIn<T>::Run(v); };
   };
}

namespace
{
   class IncompleteType;
   
   enum class Pi {
      Number = 314
   };
   
   struct ImplicitlyReflectedData {
      enum Named { One, Two, Three };

      using CTTI_POD    = Yes<>;
      using CTTI_Files  = Yes<"ASE">;
      using CTTI_Typed  = Named;
      using CTTI_Values = Values<One, Two, Three>;

      Named v = One;

      inline bool operator == (const ImplicitlyReflectedData&) const noexcept = default;
   };

   class alignas(128) ImplicitlyReflectedDataWithTraits : public ImplicitlyReflectedData {
      public:
      int member {664};
      Tags::TName<bool> anotherMember {};
      int anotherMemberArray [12] {};
      int* sparseMember {};

      inline operator int() const noexcept {
         return member;
      }

      void Create(Flow::Verb&) const {
         //++member;
      }

      void Create(Flow::Verb&) {
         ++member;
      }

      ImplicitlyReflectedDataWithTraits() = default;
      explicit ImplicitlyReflectedDataWithTraits(Pi)
         : member {314} {}

      using CTTI_Named     = Yes<"MyType">;
      using CTTI_Info      = Yes<"Info about MyType">;
      using CTTI_Files     = Yes<"txt, pdf">;
      using CTTI_Versioned = Version<2, 1>;
      using CTTI_Deep      = Yes<>;
      using CTTI_POD       = Yes<>;
      using CTTI_Nullable  = Yes<>;
      using CTTI_Pooled    = PooledBySize<250>;
      using CTTI_Concrete  = ImplicitlyReflectedData;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract  = Yes<>;
      using CTTI_Bases     = ImplicitlyReflectedData;
      using CTTI_Verbs     = Verbs::Create;
      using CTTI_MapsOnto  = int;
      using CTTI_MapsFrom  = Pi;
      using CTTI_Values    = No<>;

      using Self = ImplicitlyReflectedDataWithTraits;
      using CTTI_Members   = Members<
         &Self::member,
         &Self::anotherMember,
         &Self::anotherMemberArray,
         &Self::sparseMember
      >;
   };
   
   struct ConvertibleData : ImplicitlyReflectedData {
      int member {664};

      inline operator int() const noexcept {
         return member;
      }

      ConvertibleData() = default;
      explicit ConvertibleData(Pi)
         : member {314} {}

      using CTTI_Bases     = ImplicitlyReflectedData;
      using CTTI_MapsOnto  = int;
      using CTTI_MapsFrom  = Pi;
      using CTTI_Values    = No<>;
   };
   
   struct CheckingWhatGetsInherited : ImplicitlyReflectedDataWithTraits {
      using CTTI_Named = Yes<"CheckingWhatGetsInherited">;

      using ImplicitlyReflectedDataWithTraits::ImplicitlyReflectedDataWithTraits;
   };
   
   void FunctionForTesting(void*) {
      Logger::Verbose("Executed FunctionForTesting");
   }
}

TEMPLATE_TEST_CASE("Testing reflection of incomplete types", "[rtti]",
   void, // shouldn't compile
   nullptr_t, // shouldn't compile
   IncompleteType, // shouldn't compile
   IncompleteType*,
   IncompleteType**,
   const IncompleteType**,
   const IncompleteType* const*&,
   const IncompleteType* const* const&,
   const IncompleteType* const* const&&
) {
   using T = TestType;
   RTTI::DMeta meta = MetaDataOf<T>();
   REQUIRE(meta);
   REQUIRE(meta != nullptr);
   REQUIRE(meta.GetHash() != Hash {});
   REQUIRE(meta.GetCppName() == CppNameOf<T>());
   REQUIRE(meta.GetName() == NameOf<T>());
   REQUIRE(meta.GetInfo() == InfoOf<T>());
   REQUIRE(meta.GetFiles() == FilesOf<T>());
   REQUIRE(meta.GetSuffix() == SuffixOf<T>());
   REQUIRE(meta.GetVersionMajor() == VersionOf<T>().Major);
   REQUIRE(meta.GetVersionMinor() == VersionOf<T>().Minor);

   IF_LANGULUS_MANAGED_REFLECTION(REQUIRE(meta.GetID() != 0));
   IF_LANGULUS_MANAGED_REFLECTION(REQUIRE(meta.GetBoundaries().empty()));

   if constexpr (CT::Complete<Decay<T>>)
      REQUIRE(meta.GetOrigin() == MetaDataOf<Decay<T>>());
   else
      REQUIRE(meta.GetOrigin() == nullptr);
   
   if constexpr (CT::Complete<Deptr<T>>)
      REQUIRE(meta.GetDeptr() == MetaDataOf<Deptr<T>>());
   else
      REQUIRE(meta.GetDeptr() == nullptr);
   
   REQUIRE(meta.GetDecvqAll() == MetaDataOf<DecvqAll<T>>());
   REQUIRE(meta.GetDecvq() == MetaDataOf<Decvq<T>>());
   REQUIRE(meta.AddPtr() == nullptr);
   REQUIRE(meta.AddConst() == nullptr);
   
   REQUIRE(meta.GetSize() == sizeof(T));
   REQUIRE(meta.GetAlignment() == alignof(T));
   REQUIRE(meta.IsConstant() == false);      
   REQUIRE(meta.IsDeep() == CT::Deep<T>);
   REQUIRE(meta.IsPOD() == CT::POD<T>);
   REQUIRE(meta.IsNullable() == CT::Nullable<T>);
   REQUIRE(meta.IsAbstract() == CT::Abstract<T>);
   REQUIRE(meta.GetAllocationPage() == ::std::max(Alignment, Roof2(sizeof(T))));

   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == CT::GetPoolTactic<T>()));
   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolchain() == nullptr));
   REQUIRE(meta.HasGetHashMethod() == false);

   REQUIRE(meta.GetDefaultConstructor() != nullptr);
   REQUIRE(meta.GetDescribeConstructor() == nullptr);
   REQUIRE(meta.GetReferConstructor() != nullptr);
   REQUIRE(meta.GetCopyConstructor() != nullptr);
   REQUIRE(meta.GetDisownConstructor() != nullptr);
   REQUIRE(meta.GetCloneConstructor() != nullptr);
   REQUIRE(meta.GetMoveConstructor() != nullptr);
   REQUIRE(meta.GetAbandonConstructor() != nullptr);
   
   REQUIRE(meta.GetDestructor() != nullptr);
   REQUIRE(meta.GetComparer() != nullptr);

   REQUIRE(meta.GetReferAssigner() != nullptr);
   REQUIRE(meta.GetCopyAssigner() != nullptr);
   REQUIRE(meta.GetDisownAssigner() != nullptr);
   REQUIRE(meta.GetCloneAssigner() != nullptr);
   REQUIRE(meta.GetMoveAssigner() != nullptr);
   REQUIRE(meta.GetAbandonAssigner() != nullptr);

   REQUIRE(meta.GetResolver() == nullptr);
   REQUIRE(meta.GetHasher() == nullptr);
   REQUIRE(meta.GetReferencer() == nullptr);
   REQUIRE(meta.GetDispatcher() == nullptr);
   REQUIRE(meta.GetDispatcherMut() == nullptr);
   REQUIRE(meta.GetConcrete() == nullptr);
   REQUIRE(meta.GetProducer() == nullptr);

   REQUIRE(meta.GetMembers().size() == 0);
   REQUIRE(meta.GetAbilities().size() == 0);
   REQUIRE(meta.GetBases().size() == 0);
   REQUIRE(meta.GetMorphismsTo().size() == 0);
   REQUIRE(meta.GetMorphismsFrom().size() == 0);
   REQUIRE(meta.GetNamedValues().size() == 0);
}

SCENARIO("A type reflected with all traits", "[rtti]") {
   WHEN("ImplicitlyReflectedDataWithTraits reflected") {
      ImplicitlyReflectedDataWithTraits instance;
      auto ptrtobase = static_cast<ImplicitlyReflectedData*>(&instance);
      const size_t baseoffset = reinterpret_cast<char*>(ptrtobase) - reinterpret_cast<char*>(&instance);
      auto meta = MetaDataOf<ImplicitlyReflectedDataWithTraits>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta.GetName() == "MyType");
      REQUIRE(meta.GetInfo() == "Info about MyType");
      REQUIRE(meta.GetFiles() == "txt, pdf");
      REQUIRE(meta.GetVersionMajor() == 2);
      REQUIRE(meta.GetVersionMinor() == 1);
      REQUIRE(meta.IsDeep() == true);
      REQUIRE(meta.IsPOD() == false);       // not POD due to being abstract
      REQUIRE(meta.IsNullable() == false);  // not nullifiable due to being abstract
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == PoolTactic::Size));
      REQUIRE(meta.GetConcrete().Is(MetaDataOf<ImplicitlyReflectedData>()));
      REQUIRE(meta.GetAllocationPage() == Roof2(250 * sizeof(ImplicitlyReflectedDataWithTraits)));
      REQUIRE(meta.IsAbstract() == true);
      REQUIRE(meta.GetSize() == sizeof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta.GetAlignment() == alignof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta.GetOrigin() == meta);
      REQUIRE(meta.IsConstant() == false);
      REQUIRE(meta.GetDeptr() == nullptr);
      REQUIRE(meta.GetDecvqAll() == MetaDataOf<ImplicitlyReflectedDataWithTraits>());

      REQUIRE(meta.GetBases().size() == 1);
      REQUIRE(RTTI::DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ConvertibleData>()));
      REQUIRE(meta.GetBases()[0].imposed == false);
      REQUIRE(meta.GetBases()[0].binaryCompatible == false);
      REQUIRE(meta.GetBases()[0].count == 1);
      REQUIRE(baseoffset >= 0);
      REQUIRE(meta.GetBases()[0].offset == baseoffset);

      REQUIRE(meta.GetAbilities().size() == 1);
      auto ability = meta.GetAbilities().begin();
      REQUIRE(RTTI::VMeta(ability->first) == MetaVerbOf<Verbs::Create>());
      REQUIRE(ability->second.call != nullptr);
      REQUIRE(ability->second.callMut != nullptr);

      REQUIRE(meta.GetMembers().size() == 3);
      REQUIRE(meta.GetMembers()[0].extent == 1);
      REQUIRE(meta.GetMembers()[0].member(&instance) == &instance.anotherMember);
      REQUIRE(RTTI::TMeta(meta.GetMembers()[0].getTag(0)) == MetaTagOf<Tags::Name>());
      REQUIRE(meta.GetMembers()[0].getTag(1) == nullptr);
      REQUIRE(RTTI::DMeta(meta.GetMembers()[0].type()).Is(MetaDataOf<bool>()));

      REQUIRE(meta.GetMembers()[1].extent == 12);
      REQUIRE(meta.GetMembers()[1].member(&instance) == instance.anotherMemberArray);
      REQUIRE(meta.GetMembers()[1].getTag(0) == nullptr);
      REQUIRE(RTTI::DMeta(meta.GetMembers()[1].type()).Is(MetaDataOf<int>()));

      REQUIRE(meta.GetMembers()[2].extent == 1);
      REQUIRE(meta.GetMembers()[2].member(&instance) == &instance.sparseMember);
      REQUIRE(meta.GetMembers()[2].getTag(0) == nullptr);
      REQUIRE(RTTI::DMeta(meta.GetMembers()[2].type()).Is(MetaDataOf<int>()));

      REQUIRE(meta.GetNamedValues().size() == 0);

      const auto intmeta = RTTI::DefinitionData::Reflect<int>();
      REQUIRE(meta.GetMorphismsTo().size() == 1);
      REQUIRE(meta.GetMorphismsTo().at(intmeta).call != nullptr);
      REQUIRE(meta.GetMorphism(RTTI::DMeta(intmeta)) == meta.GetMorphismsTo().at(intmeta).call);

      const auto pimeta = RTTI::DefinitionData::Reflect<Pi>();
      REQUIRE(meta.GetMorphismsFrom().size() == 1);
      REQUIRE(meta.GetMorphismsFrom().at(pimeta).call != nullptr);
      REQUIRE(meta.GetMorphism(RTTI::DMeta(pimeta)) == nullptr);

      REQUIRE(RTTI::DMeta(pimeta).GetMorphism(meta) == meta.GetMorphismsFrom().at(pimeta).call);

      int converted = 1;
      meta.GetMorphism(RTTI::DMeta(intmeta))(&instance, &converted);
      REQUIRE(converted == 664);

      Pi source;
      ImplicitlyReflectedDataWithTraits convertedFromPi1;
      RTTI::DMeta(pimeta).GetMorphism(meta)(&source, &convertedFromPi1);
      REQUIRE(convertedFromPi1.member == 314);
   }

   WHEN("CheckingWhatGetsInherited reflected") {
      CheckingWhatGetsInherited instance;
      ImplicitlyReflectedData* ptrtobase = &static_cast<ImplicitlyReflectedData&>(instance);
      const size_t baseoffset = reinterpret_cast<char*>(ptrtobase) - reinterpret_cast<char*>(&instance);
      auto meta = MetaDataOf<CheckingWhatGetsInherited>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta.GetInfo() == "Info about MyType");
      REQUIRE(meta.GetFiles() == "txt, pdf");
      REQUIRE(meta.GetVersionMajor() == 2);
      REQUIRE(meta.GetVersionMinor() == 1);
      REQUIRE(meta.IsDeep() == true);
      REQUIRE(meta.IsPOD() == false);           // not POD due to being abstract
      REQUIRE(meta.IsNullable() == false);   // not nullifiable due to being abstract
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == PoolTactic::Size));
      REQUIRE(meta.GetConcrete().Is(MetaDataOf<ImplicitlyReflectedData>()));
      REQUIRE(meta.GetAllocationPage() == Roof2(250 * sizeof(ImplicitlyReflectedDataWithTraits)));
      REQUIRE(meta.IsAbstract() == true);
      REQUIRE(meta.GetSize() == sizeof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta.GetAlignment() == alignof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta.GetOrigin() == meta);
      REQUIRE(meta.IsConstant() == false);
      REQUIRE(meta.GetDeptr() == nullptr);
      REQUIRE(meta.GetDecvqAll() == MetaDataOf<CheckingWhatGetsInherited>());

      REQUIRE(meta.GetBases().size() == 1);
      REQUIRE(RTTI::DMeta(meta.GetBases()[0].type).Is(MetaDataOf<ConvertibleData>()));
      REQUIRE(meta.GetBases()[0].imposed == false);
      REQUIRE(meta.GetBases()[0].binaryCompatible == false);
      REQUIRE(meta.GetBases()[0].count == 1);
      REQUIRE(baseoffset >= 0);
      REQUIRE(meta.GetBases()[0].offset == baseoffset);

      REQUIRE(meta.GetAbilities().size() == 1);
      auto ability = meta.GetAbilities().begin();
      REQUIRE(RTTI::VMeta(ability->first) == MetaVerbOf<Verbs::Create>());
      REQUIRE(ability->second.call != nullptr);
      REQUIRE(ability->second.callMut != nullptr);

      REQUIRE(meta.GetMembers().size() == 0);
      REQUIRE(meta.GetNamedValues().size() == 0);

      const auto intmeta = MetaDataOf<int>();
      REQUIRE(meta->mConvertersTo.size() == 1);
      REQUIRE(meta->mConvertersTo.at(intmeta).mType->Is<int>());
      REQUIRE(meta->mConvertersTo.at(intmeta).mFunction);
      REQUIRE(meta->GetConverter(intmeta) == meta->mConvertersTo.at(intmeta).mFunction);

      const auto pimeta = MetaDataOf<Pi>();
      REQUIRE(meta->mConvertersFrom.size() == 1);
      REQUIRE(meta->mConvertersFrom.at(pimeta).mType->Is<Pi>());
      REQUIRE(meta->mConvertersFrom.at(pimeta).mFunction);
      REQUIRE(meta->GetConverter(pimeta) == nullptr);

      REQUIRE(pimeta->GetConverter(meta) == meta->mConvertersFrom.at(pimeta).mFunction);

      int converted = 1;
      meta->GetConverter(intmeta)(&instance, &converted);
      REQUIRE(converted == 664);

      Pi source;
      CheckingWhatGetsInherited convertedFromPi1;
      pimeta->GetConverter(meta)(&source, &convertedFromPi1);
      REQUIRE(convertedFromPi1.member == 314);
   }
}

SCENARIO("A simple type reflected with CTTI traits", "[rtti]") {
   WHEN("ImplicitlyReflectedData reflected") {
      auto meta = MetaDataOf<ImplicitlyReflectedData>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "ImplicitlyReflectedData");
      REQUIRE(meta->mCppName == "ImplicitlyReflectedData");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "ASE");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == true);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(ImplicitlyReflectedData));
      REQUIRE(meta->mAlignment == alignof(ImplicitlyReflectedData));

      REQUIRE(meta->mNamedValues.size() == 3);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("ForcedAbstract reflected") {
      auto meta = MetaDataOf<ForcedAbstract>();

      static_assert(not CT::HasIntentAssign<Langulus::Moved, PureVirtual>);
      static_assert(::std::assignable_from<PureVirtual&, PureVirtual&&>);

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "ForcedAbstract");
      REQUIRE(meta->mCppName == "ForcedAbstract");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(ForcedAbstract));
      REQUIRE(meta->mAlignment == alignof(ForcedAbstract));
      REQUIRE(meta->mNamedValues.empty());
      REQUIRE(meta->mConvertersTo.empty());
      REQUIRE(meta->mConvertersFrom.empty());
   }

   WHEN("PureVirtual reflected") {
      auto meta = MetaDataOf<PureVirtual>();

      static_assert(not CT::HasIntentAssign<Langulus::Moved, PureVirtual>);
      static_assert(::std::assignable_from<PureVirtual&, PureVirtual&&>);

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "PureVirtual");
      REQUIRE(meta->mCppName == "PureVirtual");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(PureVirtual));
      REQUIRE(meta->mAlignment == alignof(PureVirtual));
      REQUIRE(meta->mNamedValues.empty());
      REQUIRE(meta->mConvertersTo.empty());
      REQUIRE(meta->mConvertersFrom.empty());
   }
}

SCENARIO("A reflected verb with CTTI traits", "[rtti]") {
   GIVEN("Create verb with positive/negative tokens, with stateless and contextual default functions") {
      WHEN("Reflected") {
         Anyness::Many someBlock;
         Flow::Verb someVerb;
         auto meta = MetaVerbOf<Verbs::Create>();

         REQUIRE(meta != nullptr);
         REQUIRE(meta->mToken == "Create");
         REQUIRE(meta->mTokenReverse == "Destroy");
         REQUIRE(meta->mInfo.starts_with("Used for allocating new elements. "));
         REQUIRE(meta->mVersionMajor == 1);
         REQUIRE(meta->mVersionMinor == 0);
         REQUIRE(meta->mOperator == " + ");
         REQUIRE(meta->mPrecedence == 5);
         REQUIRE(meta->mOperatorReverse == " - ");
         REQUIRE(meta->mDefaultInvocationMutable);
         REQUIRE(meta->mDefaultInvocationMutable(someBlock, someVerb) == false);
         REQUIRE(meta->mDefaultInvocationConstant);
         REQUIRE(meta->mDefaultInvocationConstant(someBlock, someVerb) == true);
         REQUIRE(meta->mStatelessInvocation);
         REQUIRE(meta->mStatelessInvocation(someVerb) == false);
      }
   }
}

SCENARIO("A reflected function signature", "[rtti]") {
   GIVEN("A reflected function pointer") {
      using Signature = void(*)(void*);

      static_assert(    ::std::is_function_v<Deptr<Signature>>);
      static_assert(    CT::Sparse<Signature>);
      static_assert(not CT::Decayed<Signature>);
      static_assert(    CT::Complete<Signature>);

      auto meta = MetaDataOf<Signature>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "<void(void*)>*");
      REQUIRE(meta->mIsSparse);
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
   }

   /*GIVEN("A reflected function reference (shouldn't compile)") {
      auto FuncRef = FunctionForTesting;
      using Signature = decltype(*FuncRef);

      WHEN("Reflected") {
         static_assert(    ::std::is_function_v<Deref<Signature>>);
         static_assert(    CT::Dense<Signature>);
         static_assert(not CT::Decayed<Signature>);
         static_assert(not CT::Complete<Signature>);

         auto meta = MetaData::Of<Signature>();
         REQUIRE(meta != nullptr);

         THEN("Requirements should be met") {
            REQUIRE(meta->mToken == "void(*)(void*)");
            REQUIRE(meta->mIsSparse);
            REQUIRE(meta->mVersionMajor == 1);
            REQUIRE(meta->mVersionMinor == 0);
         }
      }
   }*/
}
